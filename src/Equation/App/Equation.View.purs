module Equation.View where

import Prelude (Unit, bind, map, pure, show, unit, void, ($), (<>), (>>>))
import Equation (Equation(..))
import Equation.Controller (AppState(..), EquationValidationStatus(..), Errors, InputStatus(..), initialState, updateAppStateC, updateOperandOne, updateOperandTwo, updateOperator)
import Data.Validation.Semigroup (V, unV, invalid)
import Data.Either (Either(Right, Left))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, message)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Except (runExcept)
import Data.Array (concat, (:)) as Arr
import Data.List.Types (toList)
import Data.Foldable (foldl)
import Data.Foreign (F, MultipleErrors, readString, toForeign)
import Data.Foreign.Index (prop)
import React (Event, ReactClass, ReactElement, ReactState, ReactThis, Read, Write, createClass, readState, spec, writeState)
import React.DOM as D
import React.DOM.Props as P

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
         Left ls -> invalid $ (toList >>> (foldl (\a v->(error $ show v) Arr.: a) [])) ls
         Right s -> pure s
  in eitherEventValue

updateAppStateV:: forall t83 t89 t90.
        ReactThis t83 AppState
        -> (String -> Equation)
           -> Event
              -> Eff
                   ( state :: ReactState
                                ( read :: Read
                                , write :: Write
                                | t90
                                )
                   , err :: EXCEPTION
                   , ajax :: AJAX
                   | t89
                   )
                   Unit
updateAppStateV ctx updateField event = void do
  appState@AppState {equation:oldEquation, equationValidationStatus:oldEquationValidationStatus, errors} <- readState ctx
  let vEvent::(V Errors String)
      vEvent= mapForignEventV (valueOf event)
  let showWithErrors errs =  do
        writeState ctx $ AppState {equation:oldEquation,
                                   equationValidationStatus:oldEquationValidationStatus,
                                   errors:errs}
        pure unit
  let newAppState inChar = do
        launchAff do
          res <- updateAppStateC updateField appState inChar
          liftEff $ writeState ctx res
        pure unit
  unV showWithErrors newAppState vEvent

equationReactClass :: forall props. ReactClass props
equationReactClass = createClass $ spec initialState \ctx -> do
  appState@AppState { equation: equat'@(Equation equat), equationValidationStatus: EquationValidationStatus eqerrs, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text $ message err ]

      renderValidationErrors [] = []
      renderValidationErrors xs =
        [ D.div [ P.className "alert alert-danger" ]
                [ D.ul' (map renderValidationError xs) ]
        ]

      classesAtStatus::InputStatus->String
      classesAtStatus NoInput = ""
      classesAtStatus SuccessOnInput = ""
      classesAtStatus (WarningOnInput _) = "has-warning"
      classesAtStatus (ErrorOnInput _) = "has-error"
      --

      elementsAtStattus::InputStatus->Array ReactElement
      elementsAtStattus NoInput = []
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

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Equation" ]

                           , formField "operand 1" "operand 1" equat.o1 (updateOperandOne equat') eqerrs.o1
                           , formField "operator"  "operator"  equat.op (updateOperator   equat') eqerrs.op
                           , formField "operand 2" "operand 2" equat.o2 (updateOperandTwo equat') eqerrs.o2
                           , D.label [ P.className "col-sm-2 control-label" ]
                                 [D.text "result"]
                               , D.div [ P.className "col-sm-3" ]
                                       [D.text $ equat.res]
                             ]
                  ]
          ]
