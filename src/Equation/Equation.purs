module Equation where

import Prelude (class Show, (<>))

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