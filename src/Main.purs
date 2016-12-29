module Main where

import Prelude (class Show, Unit, bind, unit, void, pure, map, show, (>>=), ($), (<>), (<$>), (>>>))

-- import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Int (fromString)
import Data.Traversable (for)
import Data.Validation.Semigroup (V, unV, invalid)
import Data.Maybe (Maybe(Just), fromJust, maybe)
import Data.Either (Either(Right, Left))
import Data.Array ((:))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Aff (Canceler, later, runAff)
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

data InputStatus = NoInput | SuccessOnInput | WarningOnInput String | ErrorOnInput String
instance showInputSatus::Show InputStatus where
  show NoInput            = "no input"
  show (SuccessOnInput  ) = "success on input"
  show (WarningOnInput s) = "Warning: " <> s
  show (ErrorOnInput   s) = "Error: " <> s

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

type Errors = Array String

newtype AppState = AppState
  { equation :: Equation
  , equationValidationStatus :: EquationValidationStatus
  , errors::Errors
  }

initialState :: AppState
initialState = AppState
  { equation: equation "" "" "" ""
  , equationValidationStatus: equationValidationStatus NoInput NoInput NoInput
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
  AppState {equation:oldEquation, equationValidationStatus:oldEquationValidationStatus, errors} <- readState ctx
  let vEvent::(V Errors String)
      vEvent= mapForignEventV (valueOf e)
  let vUpdatedEquationOrError::V Errors Equation
      vUpdatedEquationOrError = updateField <$> vEvent
  let updatedEquation::Equation
      updatedEquation = unV (\_->oldEquation) (\e->e) vUpdatedEquationOrError
  let otherErrors::Errors
      otherErrors = unV (\e->e) (\_->errors) vUpdatedEquationOrError
  let newEquationValidationStatus::EquationValidationStatus
      newEquationValidationStatus = validateEquation updatedEquation
  let updateAppState (eq::Equation) (eqErrs::EquationValidationStatus) (errs::Errors) = do  
        writeState ctx (AppState { equation: eq, equationValidationStatus:eqErrs, errors: errs })
        pure unit
  let requestResult::Equation->EquationValidationStatus->Errors->Eff ( ajax::AJAX
         , console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
      requestResult eq@(Equation {o1:o1,op:op,o2:o2}) eqerrs errs= do 
                  log "in requestResult"
                  for errs log
                  unV
                     (\_ -> do 
                         log "no valid input for request"
                         updateAppState (Equation {o1:o1,op:op,o2:o2,res:o1 <> op <> o2}) eqerrs errs 
                         pure unit)
                     (\(url::URL) -> do 
                         log "input valid for request"
                         -- first output the raw equation
                         updateAppState (Equation {o1:o1,op:op,o2:o2,res:o1 <> op <> o2}) eqerrs errs
                         -- asyncronously compute the result
                         makeRequest (\error->do
                                         log "request wirh error"
                                         updateAppState (Equation {o1:o1,op:op,o2:o2,res:o1 <> op <> o2}) eqerrs (show error : otherErrors)) 
                                     (\res ->do
                                         log $ "request without error with result: " <> res.response
                                         updateAppState (equation o1 op o2 res.response) eqerrs errs) 
                                     url
                         pure unit)
                     (makeUrlV o1 op o2)
                  -- pure unit
  requestResult updatedEquation newEquationValidationStatus otherErrors
  
equationReactClass :: forall props. ReactClass props
equationReactClass = createClass $ spec initialState \ctx -> do
  AppState { equation: Equation equat, equationValidationStatus: EquationValidationStatus eqerrs, errors } <- readState ctx

  let renderValidationError err = D.li' [ D.text err ]

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

      updateOperandOne s = Equation $ equat { o1 = s, res = s <> equat.op <> equat.o2  }
      updateOperandTwo s = Equation $ equat { o2 = s, res = equat.o1 <> equat.op <> s }
      updateOperator   s = Equation $ equat { op = s , res = equat.o1 <> s <> equat.o2 }
      updateResult     s = Equation $ equat { res = s <> equat.o1 <> equat.op <> equat.o2 }
      
  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  (renderValidationErrors errors)
          , D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Equation" ]

                           , formField "operand 1" "operand 1" equat.o1 updateOperandOne eqerrs.o1
                           , formField "operator" "operator" equat.op updateOperator $ eqerrs.op
                           , formField "operand 2" "operand 2" equat.o2 updateOperandTwo $ eqerrs.o2
                           , D.label [ P.className "col-sm-2 control-label" ]
                                 [D.text "result"]
                               , D.div [ P.className "col-sm-3" ] 
                                       [D.text $ equat.res]
                             ]
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