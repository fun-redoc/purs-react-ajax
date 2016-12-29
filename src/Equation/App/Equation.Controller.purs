module Equation.Controller where

import Prelude (class Show, Unit, bind, unit, void, pure, map, show,
                (>>=), ($), (<>), (<$>), (>>>), (<<<), id, (*>))
import Equation

-- import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Int (fromString)
import Data.Traversable (for)
import Data.Validation.Semigroup (V, unV, invalid, isValid)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromJust)
import Data.Either (Either(Right, Left), either)
import Data.Array ((:))
import Data.Bifunctor (bimap, lmap, rmap)

import Control.Monad.State
import Control.Monad.State.Class

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, error)
--import Control.Monad.Aff (Aff, Canceler, launchAff, runAff, attempt, later)
import Control.Monad.Aff (Aff, Canceler, attempt, later)
import Control.Monad.Aff.Class (liftAff)
import Network.HTTP.Affjax (URL, AJAX, get)
import Network.HTTP.StatusCode (StatusCode)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.Affjax.Response (class Respondable)

import Control.Monad.Except (runExcept)
import Data.Array (concat, (:)) as Arr
import Data.List.Types (toList)
import Data.Foldable (foldl)
import Data.Foreign (F, MultipleErrors, readString, toForeign)
import Data.Foreign.Index (prop)
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


import Control.Monad.RWS as R
import Data.Tuple as T

data InputStatus = NoInput | SuccessOnInput | WarningOnInput String | ErrorOnInput String
instance showInputSatus::Show InputStatus where
  show NoInput            = "no input"
  show (SuccessOnInput  ) = "success on input"
  show (WarningOnInput s) = "Warning: " <> s
  show (ErrorOnInput   s) = "Error: " <> s
newtype  EquationValidationStatus =  EquationValidationStatus {
  o1  :: InputStatus,
  op  :: InputStatus,
  o2  :: InputStatus
}

equationValidationStatus::InputStatus->InputStatus->InputStatus->EquationValidationStatus
equationValidationStatus o1 op o2 = EquationValidationStatus { o1:o1, op:op, o2:o2 }
instance showEquationValidationStatus::Show EquationValidationStatus where
  show e@(EquationValidationStatus {o1:o1,op:op,o2:o2}) =
    show o1 <> "\n" <> show op <> "\n" <> show o2 <> "\n"

validateEquation :: Equation -> EquationValidationStatus
validateEquation eq@(Equation {o1:o1,op:op,o2:o2,res:res}) =
    equationValidationStatus (matchesNumber "first operand" o1) (nonEmpty "operatoer" op) (matchesNumber "second operand" o2)


nonEmpty :: String -> String -> InputStatus
nonEmpty field "" = ErrorOnInput $ "Field '" <> field <> "' cannot be empty"
nonEmpty _     _  = SuccessOnInput

matches :: String -> Either String Regex -> String -> InputStatus
matches _  (Right regex) value | test regex value = SuccessOnInput
matches field (Right _)  _     = ErrorOnInput $ "Field '" <> field <> "' did not match the required format"
matches field (Left err) _     = WarningOnInput $ "Field '" <> field <> "' error in format definition:" <> err
-- matches field _     _     = Just $ "Field '" <> field <> "' did not match the required format"

matchesNumber::String -> String -> InputStatus
matchesNumber field value =
  matches field (regex """\d+""" noFlags) value

type Errors = Array Error

newtype AppState = AppState
  { equation :: Equation
  , equationValidationStatus :: EquationValidationStatus
  , errors::Errors
  }
makeAppState::Equation->EquationValidationStatus->Errors->AppState
makeAppState (eq::Equation) (eqErrs::EquationValidationStatus) (errs::Errors) =
  AppState { equation: eq, equationValidationStatus:eqErrs, errors: errs }

initialState :: AppState
initialState = AppState
  { equation: equation "" "" "" ""
  , equationValidationStatus: equationValidationStatus NoInput NoInput NoInput
  , errors: []
  }


updateOperandOne (Equation equat) s = Equation $ equat { o1 = s, res = s <> equat.op <> equat.o2  }
updateOperandTwo (Equation equat) s = Equation $ equat { o2 = s, res = equat.o1 <> equat.op <> s }
updateOperator   (Equation equat) s = Equation $ equat { op = s , res = equat.o1 <> s <> equat.o2 }
updateResult     (Equation equat) s = Equation $ equat { res = s }


makeUrlV::String->String->String->V Errors URL
makeUrlV o1 op o2 =
  maybe (invalid [error $ o1 <> op <> o2]) (pure) maybeUrl
  where
      urlOp = "/add/"
      maybeInt1::Maybe Int
      maybeInt1 = fromString o1
      maybeInt2::Maybe Int
      maybeInt2 = fromString o2
      maybeUrl::Maybe URL
      maybeUrl = maybeInt1 >>= (\i1 -> maybeInt2) >>= (\i2 -> Just $ "/calc/" <> o1 <> urlOp <> o2)

updateAppStateC updateFieldFn appState inChar = do
  let updatedEquation = updateFieldFn inChar
  let newEquationValidationStatus::EquationValidationStatus
      newEquationValidationStatus = validateEquation updatedEquation
  let addRemote:: Equation -> Aff _ (V Errors (Maybe Int))
      addRemote (Equation {o1,op,o2,res}) = do
        let urlV::(V Errors String)
            urlV = makeUrlV o1 op o2
        let eitherToV::Either Error (Maybe Int) -> V Errors (Maybe Int)
            eitherToV (Left e) = invalid [e]
            eitherToV (Right r) = pure r
        let request::String-> Aff _ (V Errors (Maybe Int))
            request url = do responseE <- attempt $ (fromString <<< _.response) <$> (get url)
                             pure $ eitherToV responseE
        unV
          (\err -> pure $ invalid err)
          (\url -> request url)
          urlV
  res <- addRemote updatedEquation
  pure $
    unV
      (\errors   -> makeAppState updatedEquation newEquationValidationStatus errors)
      (\maybeInt -> makeAppState
                      (maybe updatedEquation (show >>> updateResult updatedEquation) maybeInt)
                      newEquationValidationStatus
                      [])
      (res)
