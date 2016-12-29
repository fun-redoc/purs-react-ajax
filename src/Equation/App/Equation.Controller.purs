module Equation.Controller where

import Prelude (class Show, bind, pure, show, ($), (<$>), (<<<), (<>), (>>=), (>>>))
import Equation (Equation(..), equation)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Int (fromString)
import Data.Validation.Semigroup (V, invalid, unV)
import Data.Maybe (Maybe(Just), maybe)
import Data.Either (Either(Right, Left))
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Network.HTTP.Affjax (AJAX, URL, get)

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


updateOperandOne :: Equation -> String -> Equation
updateOperandOne (Equation equat) s = Equation $ equat { o1 = s, res = s <> equat.op <> equat.o2  }
updateOperandTwo :: Equation -> String -> Equation
updateOperandTwo (Equation equat) s = Equation $ equat { o2 = s, res = equat.o1 <> equat.op <> s }
updateOperator :: Equation -> String -> Equation
updateOperator   (Equation equat) s = Equation $ equat { op = s , res = equat.o1 <> s <> equat.o2 }
updateResult :: Equation -> String -> Equation
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

updateAppStateC :: forall t119 t120 t156.
  (t120 -> Equation)
  -> t119
     -> t120
        -> Aff
             ( ajax :: AJAX
             | t156
             )
             AppState
updateAppStateC updateFieldFn appState inChar = do
  let updatedEquation = updateFieldFn inChar
  let newEquationValidationStatus::EquationValidationStatus
      newEquationValidationStatus = validateEquation updatedEquation
  let addRemote::forall t1560. Equation -> Aff ( ajax :: AJAX | t1560 ) (V Errors (Maybe Int))
      addRemote (Equation {o1,op,o2,res}) = do
        let urlV::(V Errors String)
            urlV = makeUrlV o1 op o2
        let eitherToV::Either Error (Maybe Int) -> V Errors (Maybe Int)
            eitherToV (Left e) = invalid [e]
            eitherToV (Right r) = pure r
        let request::forall t15603. String-> Aff ( ajax :: AJAX | t15603 ) (V Errors (Maybe Int))
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
