module Main where

import Prelude

import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Int (fromString)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (maybe)
import Data.Traversable (for)
import Data.Bifunctor (lmap, rmap)

import Data.Show -- (Show)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, message)
import Control.Monad.Aff (Aff, Canceler, launchAff, runAff, later)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (AffjaxResponse, AJAX, URL, get, defaultRequest, affjax)
import Network.HTTP.StatusCode (StatusCode)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.Affjax.Response -- (Respondable(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Array  ((..), (:),concat, length, modifyAt, zipWith) as Arr
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
import React (ReactElement, ReactClass, ReadWrite, ReactState, Event, ReactThis,
              createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

data InputStatus = SuccessOnInput | WarningOnInput String | ErrorOnInput String

newtype  Equation =  Equation {
  o1  :: String,
  op  :: String,
  o2  :: String,
  res :: String
}
instance showEquation::Show Equation where
  show e@(Equation {o1:o1,op:op,o2:o2,res:res}) = 
    o1 <> " " <> op <> " " <> o2 <> " " <> " = " <> res
equation::String->String->String->String->Equation
equation o1 op o2 res = Equation { o1:o1, op:op, o2:o2, res:res }

maybeOp::String -> Maybe (Int->Int->Int)
maybeOp "+" = Just (\o1 o2 -> o1 + o2)
maybeOp "*" = Just (\o1 o2 -> o1 * o2)
maybeOp   _ = Nothing

type Errors = Array String

validateEquation :: Equation -> V Errors Equation
validateEquation (Equation o) =
  nonEmptyEquation (Equation o)
--  equation <$> (nonEmpty "first operand" o.o1 *> pure o.o1)
--           <*> (nonEmpty "operator"  o.op  *> pure o.op)
--           <*> (nonEmpty "second operand"  o.o2  *> pure o.o2)
--           <*> (nonEmpty "result"  o.res  *> pure o.res)

nonEmptyEquation::Equation -> V Errors Equation
nonEmptyEquation eq@(Equation {o1:o1,op:op,o2:o2,res:res}) = 
  if length o1 /= 0 && length o2 /= 0 && length op == 0 then
    invalid ["Operator cannot be empty"] *> pure eq
  else 
    -- pure eq 
    equation <$> (matchesNumber "first operand" o1 *> pure o1)
             <*> (pure op)
             <*> (matchesNumber "second operand" o2 *> pure o2)
             <*> (pure res)
    
nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

matches :: String -> Either String Regex -> String -> V Errors Unit
matches _  (Right regex) value | test regex value = pure unit
matches field (Right _)  _     = invalid ["Field '" <> field <> "' did not match the required format"]
matches field (Left err) _     = invalid ["Field '" <> field <> "' error in format definition:" <> err]
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

matchesNumber::String -> String -> V Errors Unit
matchesNumber field value = 
  matches field (regex """\d+""" noFlags) value

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

mapForignEventV::F String -> V Errors String
mapForignEventV fe = 
  let eitherEventValueForign::Either MultipleErrors String
      eitherEventValueForign = runExcept fe
      eitherEventValue::V Errors String
      eitherEventValue = case eitherEventValueForign of
         Left ls -> invalid $ (toList >>> (foldl (\a v->(show v) Arr.: a) [])) ls
         Right s -> pure s 
  in eitherEventValue
  
  
makeRequest::forall res eff. (Respondable res) => 
    (Error -> Eff ( ajax :: AJAX | eff ) Unit )  
 -> ({ status :: StatusCode, headers :: Array ResponseHeader, response :: res} -> Eff (ajax::AJAX | eff) Unit)  
 -> String 
 -> Eff (ajax :: AJAX | eff) (Canceler (ajax :: AJAX | eff))
makeRequest onError' onSuccess' url = 
 runAff onError' onSuccess' $ later $ get url
 
makeUrlV::String->String->String->V Errors URL
makeUrlV o1 op o2 = 
  maybe (invalid ["wrong format"]) (pure) maybeUrl
  where 
      urlOp = "/add/"
      maybeInt1::Maybe Int
      maybeInt1 = fromString o1
      maybeInt2::Maybe Int
      maybeInt2 = fromString o2
      maybeUrl::Maybe URL
      maybeUrl = maybeInt1 >>= (\i1 -> maybeInt2) >>= (\i2 -> Just $ "/calc/" <> o1 <> urlOp <> o2)
  
  
updateAppStateV
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Equation)
  -> Event
  -> Eff ( ajax::AJAX
         , console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppStateV ctx updateField e = void do
  AppState {equation:oldEquation, errors} <- readState ctx
  let vEvent::(V Errors String)
      vEvent= mapForignEventV (valueOf e)
  let vUpdatedEquationOrError::V Errors Equation
      vUpdatedEquationOrError = updateField <$> vEvent
  let updatedEquation::Equation
      updatedEquation = unV (\_->oldEquation) (\e->e) vUpdatedEquationOrError
  let vValidatedEquationOrErrors::V Errors Equation
      vValidatedEquationOrErrors = validateEquation updatedEquation
  let failedUpdate (errs::Errors) = do  for errs log
                                        writeState ctx (AppState { equation: updatedEquation, errors: errs })
                                        pure unit
  let successRequest (Equation eq) res = do 
                                 let ne = eq {res=res}
                                 writeState ctx (AppState { equation: Equation ne, errors: [] })
                                 pure unit
  let failedRequest (err::Error) =  do log $ show err
                                       writeState ctx (AppState { equation: updatedEquation, errors: [message err] })
                                       pure unit
  let requestResult::Equation->Eff ( ajax::AJAX
         , console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
      requestResult eq@(Equation {o1:o1,op:op,o2:o2}) = do 
                  log $ show eq
                  unV
                     (\_ -> do 
                         successRequest eq $ o1 <> op <> o2
                         pure unit)
                     (\(url::URL) -> do 
                         -- first output the raw equation
                         successRequest eq $ o1 <> op <> o2
                         -- asyncronously comoute the result
                         makeRequest failedRequest (\res -> successRequest eq res.response) url
                         pure unit)
                     (makeUrlV o1 op o2)
                  -- pure unit
  unV failedUpdate requestResult vValidatedEquationOrErrors
  
equationReactClass :: forall props. ReactClass props
equationReactClass = createClass $ spec initialState \ctx -> do
  AppState { equation: Equation equat, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text err ]

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]
        
      classesAtStatus::InputStatus->String
      classesAtStatus SuccessOnInput = ""
      classesAtStatus (WarningOnInput _) = "has-warning"
      classesAtStatus (ErrorOnInput _) = "has-error"
      
      elementsAtStattus::InputStatus->Array ReactElement
      elementsAtStattus SuccessOnInput = []
      elementsAtStattus (WarningOnInput s) = 
              [ 
                D.span [ P.className "help-block"] [ D.text s]
              ]
      elementsAtStattus (ErrorOnInput s) = 
              [ 
                D.span [ P.className "help-block"] [ D.text s]
              ]

      formField name hint value update status =
        D.div [ P.className $ "form-group " <> (classesAtStatus status) ] 
              [ D.label [ P.className "col-sm-2 control-label" ]
                        [ D.text name ]
              , D.div [ P.className "col-sm-3" ] $
                      Arr.concat [[ D.input [ P._type "text"
                                            , P.className "form-control"
                                            , P.placeholder hint
                                            , P.value value
                                            , P.onChange (updateAppStateV ctx update)
                                            ] []
                                  ]
                                  , (elementsAtStattus status)
                                 ]
              ]

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

                           , formField "operand 1" "operand 1" equat.o1 updateOperandOne SuccessOnInput
                           , formField "operator" "operator" equat.op updateOperator $ ErrorOnInput "(error)"
                           , formField "operand 2" "operand 2" equat.o2 updateOperandTwo $ WarningOnInput "(warning)"
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