{-# LANGUAGE ExistentialQuantification #-}
import Control.Exception
import Data.Typeable

data MyException = forall a. Show a => MyException a deriving (Typeable)
instance Show MyException where
    show (MyException a) = "MyException: " ++ show a
instance Exception MyException where

data SomeAException = forall a. Show a => SomeAException a deriving (Typeable)
instance Show SomeAException where
    show (SomeAException a) = "SomeAException: " ++ show a
instance Exception SomeAException where

data SomeBException = forall a. Show a => SomeBException a deriving (Typeable)
instance Show SomeBException where
    show (SomeBException a) = "SomeBException: " ++ show a
instance Exception SomeBException where
