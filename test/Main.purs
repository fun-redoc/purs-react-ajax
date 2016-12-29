<<<<<<< HEAD
module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
=======
module Test.Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit (suite, test)  
import Test.Unit.Assert (equal,assertFalse)  
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Control.Monad.Aff.AVar (AVAR)

import Equation.Controller
import Control.Monad.RWS (runRWS)
import Control.Monad.RWS.Trans (RWSResult(..))
import Data.Tuple (Tuple(..), fst, snd)

main ::  forall t1. Eff
        ( console :: CONSOLE
        , testOutput :: TESTOUTPUT
        , avar :: AVAR
        | t1
        )
        Unit
main = runTest do
 suite "RWS" do
   test "test the test" do
     equal 1 1
--   test "match correct word" do
--      let input = "hello"
--      let expected = input
--      let result = case (runRWS (gameStep input) 
--                           (GameEnvironment { wordToGuess:expected ,maxGuesses:1,debugMode:true })
--                           (GameState {currentGuess:"" , guesses:0 })) of
--                RWSResult (GameState st) b w -> st.currentGuess
--      equal result expected 
--   test "fail on wrong word" do
--      let input = "hello"
--      let expected = "test"
--      let result = case (runRWS (gameStep input) 
--                           (GameEnvironment { wordToGuess:expected ,maxGuesses:1,debugMode:true })
--                           (GameState {currentGuess:"" , guesses:0 })) of
--                RWSResult (GameState st) b w -> Tuple st.currentGuess b 
--      equal (snd result) false 
--      equal (fst result) input 
--      assertFalse "state and expected word should be different" ((fst result) == expected )
>>>>>>> version
