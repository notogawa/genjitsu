{-# LANGUAGE FlexibleContexts #-}
module Sample ( sample1IO, sample2IO ) where

import qualified Prelude as P
import Prelude.Genjitsu
import Data.Typeable
import Control.Eff
import Control.Eff.Lift

-- $setup
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> :set -XDeriveFunctor
-- >>> :set -XDeriveDataTypeable
-- >>> import Control.Applicative
-- >>> import Control.Monad.State
-- >>> import Control.Monad.Writer
--
-- doctest -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d \
--   -isrc test/sample.hs
-- Examples: 14  Tried: 14  Errors: 0  Failures: 0
--


-- | サンプル1
--
-- >>> newtype Mock1 a = Mock1 { runMock1 :: State String a } deriving (Functor, Applicative, Monad, MonadState String, Typeable)
-- >>> instance MonadIOAllowReadLn   Mock1 where allowReadLn   = fmap read get
-- >>> instance MonadIOAllowPutStrLn Mock1 where allowPutStrLn = put
-- >>> runState (runMock1 $ runLift sample1) "2"
-- ((),"3")
--
sample1 :: ( Typeable1 m
           , MonadIOAllowReadLn m
           , MonadIOAllowPutStrLn m
           , Member (Lift m) r
           , SetMember Lift (Lift m) r
           ) =>
           Eff r ()
sample1 = do
  n <- readLn
  let s = n + 1 :: Int
  -- putStr "is denied"
  putStrLn (show s)

sample1IO :: IO ()
sample1IO = runLift sample1

-- | サンプル2
--
-- >>> newtype Mock2 a = Mock2 { runMock2 :: Writer String a } deriving (Functor, Applicative, Monad, MonadWriter String, Typeable)
-- >>> instance MonadIOAllowPutStr   Mock2 where allowPutStr   = tell
-- >>> instance MonadIOAllowPutStrLn Mock2 where allowPutStrLn = tell . (++"\n")
-- >>> runWriter $ runMock2 $ runLift sample2
-- ((),"Start...Done\n")
--
sample2 :: ( Typeable1 m
           , MonadIOAllowPutStr m
           , MonadIOAllowPutStrLn m
           , Member (Lift m) r
           , SetMember Lift (Lift m) r
           ) =>
           Eff r ()
sample2 = do
  putStr "Start..."
  putStrLn "Done"

sample2IO :: IO ()
sample2IO = runLift sample2
