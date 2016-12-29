module Simulate where

import Prelude
import Control.Monad.Eff (Eff, forE, whileE)
import Control.Monad.ST (ST, pureST, newSTRef, readSTRef, modifySTRef, runST, writeSTRef)
import Control.Apply (lift2)

import Control.Monad.Eff.Console (CONSOLE, log, logShow)

-- whileE :: forall e a. Eff e Boolean -> Eff e a -> Eff e Unit

-- main = runST (do
--   rn1 <- newSTRef 1
--   rn2 <- newSTRef 1
--   whileE (pure true) $ do
--     n1 <- readSTRef rn1
--     n2 <- readSTRef rn2
--     writeSTRef rn2 $ n1 + n2
--     writeSTRef rn1 n2
--     logShow n2)
--     

cnt v = pureST (do
  refI <- newSTRef 1
  whileE (lift2 (<) (readSTRef refI) (pure v)) $ do
      modifySTRef refI $ (+) 1
  readSTRef refI )
  
  
cnt' v = runST (do
  refI <- newSTRef 1
  whileE ((<) <$> (readSTRef refI) <*> (pure v)) $ do
      modifySTRef refI $ (+) 1
  readSTRef refI )
