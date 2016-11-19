module Main where

import Prelude

import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
-- import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
-- import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array  ((..), (:), length, modifyAt, zipWith) as Arr
import Data.List.Types (toList)
import Data.Either (Either(..), either)
import Data.Foldable (for_, foldl)
import Data.Foreign (F, MultipleErrors, readString, toForeign)
import Data.Foreign.Index (prop)
import Data.Maybe (fromJust, fromMaybe)
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
      -- eitherEventValue = either (toList >>> (foldl (\a v->(show v) Arr.: a) [])) (\x->x) eitherEventValueForign
      eitherEventValue = case eitherEventValueForign of
         Left ls -> Left $ (toList >>> (foldl (\a v->(show v) Arr.: a) [])) ls
         Right s -> Right s 
  in eitherEventValue
  
updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Equation)
  -> Event
  -> Eff ( console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppState ctx update e = do
--  for_ (valueOf e) \s -> do
--   let eitherEventValueForign::Either MultipleErrors String
--       eitherEventValueForign = runExcept (valueOf e)
--       eitherEventValue::Either Errors String
--       eitherEventValue = either (toList >>> (foldl (\a v->(show v) Arr.: a) [])) (\x->x) eitherEventValueForign
--       eitherNewEquation = update <$> eitherEventValue
  let eitherEventValue::Either Errors String
      eitherEventValue = mapForignEvent (valueOf e)
      eitherNewEquation::Either Errors Equation
      eitherNewEquation = update <$> eitherEventValue
  AppState {equation:oldEquation, errors} <- readState ctx
  either 
     (\lv -> writeState ctx (AppState { equation: oldEquation, errors: lv }))
     (\ne -> writeState ctx (AppState { equation: ne, errors: [] }))
     eitherNewEquation 
     
--   case eitherNewEquation >>= validateEquation' of
--     Left errors -> writeState ctx (AppState { equation: ctx.equation, errors: errors })
--     Right newEquation -> writeState ctx (AppState { equation: newEquation, errors: [] })

equationReactClass :: forall props. ReactClass props
equationReactClass = createClass $ spec initialState \ctx -> do
  -- AppState { person: Person person@{ homeAddress: Address address }, errors } <- readState ctx
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
      calc e = e.o1 <> e.op <> e.o2

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