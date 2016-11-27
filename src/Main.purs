module Main where

import Prelude

import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Int (fromString)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

import Data.Show -- (Show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, message)
import Control.Monad.Aff (Aff, Canceler, launchAff, runAff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (AffjaxResponse, AJAX, URL, get, defaultRequest, affjax)
import Network.HTTP.StatusCode (StatusCode)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.Affjax.Response -- (Respondable(..))

import Control.Monad.Aff (launchAff)
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (affjax, defaultRequest)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Array  ((..), (:), length, modifyAt, zipWith) as Arr
import Data.List.Types (toList)
import Data.Either (Either(..), either)
import Data.Foldable (for_, foldl)
import Data.Foreign (F, MultipleErrors, readString, toForeign)
import Data.Foreign.Index (prop)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Nullable (toMaybe)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis,
              createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

newtype  Equation =  Equation {
  o1  :: String,
  op  :: String,
  o2  :: String,
  res :: String
}
equation::String->String->String->String->Equation
equation o1 op o2 res = Equation { o1:o1, op:op, o2:o2, res:res }

maybeOp::String -> Maybe (Int->Int->Int)
maybeOp "+" = Just (\o1 o2 -> o1 + o2)
maybeOp "*" = Just (\o1 o2 -> o1 * o2)
maybeOp   _ = Nothing

type Errors = Array String

validateEquation :: Equation -> V Errors Equation
validateEquation (Equation o) =
  equation <$> (nonEmpty "first operand" o.o1 *> pure o.o1)
           <*> (nonEmpty "operator"  o.op  *> pure o.op)
           <*> (nonEmpty "second operand"  o.o2  *> pure o.o2)
           <*> (nonEmpty "result"  o.res  *> pure o.res)

validateEquation' :: Equation -> Either Errors Equation
validateEquation' p = unV Left Right $ validateEquation p

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

newtype AppState = AppState
  { equation :: Equation
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { equation: equation "" "" "" ""
  , errors: []
  }

valueOf :: Event -> F String
valueOf e = do
  target <- prop "target" (toForeign e)
  value <- prop "value" target
  readString value

mapForignEvent::F String -> Either Errors String
mapForignEvent fe = 
  let eitherEventValueForign::Either MultipleErrors String
      eitherEventValueForign = runExcept fe
      eitherEventValue::Either Errors String
      eitherEventValue = case eitherEventValueForign of
         Left ls -> Left $ (toList >>> (foldl (\a v->(show v) Arr.: a) [])) ls
         Right s -> Right s 
  in eitherEventValue
  
  
calcEitherEquation::Either Errors Equation->Either Errors Equation
calcEitherEquation leftVal@Left _ = leftVal
calcEitherEquation rightVal@(Right (Equation {o1, op, o2, res})) = 
  let calc::String->String->String->Either Errors Int 
      calc o1' op' o2' = 
        let maybeO1 = fromString o1'
            maybeO2 = fromString o2'
            mayBeResult = maybeOp op <*> maybeO1 <*> maybeO2
         in case mayBeResult of
              Nothing -> Left ["there is an error in the input"]
              Just i -> Right i
      result::Either Errors Int -> Either Errors Equation
      result (Left errors) = rightVal
      result (Right i)     = Right $ equation o1 op o2 $ show i
   in result $ calc o1 op o2

makeRequest::forall res eff. (Respondable res) => 
    (Error -> Eff ( ajax :: AJAX | eff ) Unit )  
 -> ({ status :: StatusCode, headers :: Array ResponseHeader, response :: res} -> Eff (ajax::AJAX | eff) Unit)  
 -> String 
 -> Eff (ajax :: AJAX | eff) (Canceler (ajax :: AJAX | eff))
makeRequest onError' onSuccess' url = 
 runAff onError' onSuccess' $ get url
 
makeUrl::String->String->String->Either Errors URL
makeUrl o1 op o2 = 
  Right $ "/calc/" <> o1 <> urlOp <> o2 
  where 
      urlOp = "/add/"
      maybeInt1::Maybe Int
      maybeInt1 = fromString o1
      maybeInt2::Maybe Int
      maybeInt2 = fromString o2
      maybeInts::Maybe Unit
      maybeInts = maybeInt1 >>= (\i1 -> maybeInt2) >>= (\i2 -> Just unit)
  
updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Equation)
  -> Event
  -> Eff ( ajax::AJAX
         , console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppState ctx updateField e = void do
  AppState {equation:oldEquation, errors} <- readState ctx
  let eitherNewEquationOrErrors = updateField <$> mapForignEvent (valueOf e)
  let failedUpdate (errs::Errors) = do 
                                      writeState ctx (AppState { equation: oldEquation, errors: errs })
                                      pure unit
  let successRequest (Equation eq) res = do 
                                 let ne = eq {res=res.response}
                                 writeState ctx (AppState { equation: Equation ne, errors: [] })
                                 pure unit
  let failedRequest (err::Error) =  do log $ show err
                                       writeState ctx (AppState { equation: oldEquation, errors: [message err] })
                                       pure unit
  let requestResult eq@(Equation {o1:o1,o2:o2}) = do 
                     makeRequest failedRequest (successRequest eq) $ "/calc/" <> o1 <> "/add/" <> o2
                     pure unit
  either failedUpdate requestResult eitherNewEquationOrErrors

equationReactClass :: forall props. ReactClass props
equationReactClass = createClass $ spec initialState \ctx -> do
  AppState { equation: Equation equat, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text err ]

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]

      formField name hint value update =
        D.div [ P.className "form-group" ]
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder hint
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
              ]

--       renderPhoneNumber (PhoneNumber phone) index =
--         formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
--           Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }
-- 
      updateOperandOne s = Equation $ equat { o1 = s, res = s <> equat.op <> equat.o2  }
      updateOperandTwo s = Equation $ equat { o2 = s, res = equat.o1 <> equat.op <> s }
      updateOperator   s = Equation $ equat { op = s , res = equat.o1 <> s <> equat.o2 }
      updateResult     s = Equation $ equat { res = s <> equat.o1 <> equat.op <> equat.o2 }
      calc e = e.res -- e.o1 <> e.op <> e.o2

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Equation" ]

                           , formField "operand 1" "operand 1" equat.o1 updateOperandOne
                           , formField "operator" "operator" equat.op updateOperator
                           , formField "operand 2" "operand 2" equat.o2 updateOperandTwo
                           , D.label [ P.className "col-sm-2 control-label" ]
                                 [D.text "result"]
                               , D.div [ P.className "col-sm-3" ] 
                                       [D.text $ calc equat]
                             ]
                           
                           -- <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                  ]
          ]

main :: Eff ( console :: CONSOLE
            , dom :: DOM
            ) Unit
main = void do
  log "Rendering address book component"
  let component = D.div [] [ createFactory equationReactClass unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust (toMaybe ctr))