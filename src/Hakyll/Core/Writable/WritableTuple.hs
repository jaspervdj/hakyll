-- | This module exposes a writable type 'WritableTuple' which is a simple
-- newtype wrapper around a tuple.
--
-- The idea is that, given a tuple @(a, b)@, @a@ is the value you actually want
-- to save to the disk, and @b@ is some additional info that you /don't/ want to
-- save, but that you need later, for example in a 'require' clause.
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Hakyll.Core.Writable.WritableTuple
    ( WritableTuple (..)
    , writableTupleFst
    , writableTupleSnd
    , writableTupleCompiler
    ) where

import Control.Arrow (arr)

import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Hakyll.Core.Writable
import Hakyll.Core.Compiler

newtype WritableTuple a b = WritableTuple {unWritableTuple :: (a, b)}
                          deriving (Show, Eq, Ord, Binary, Typeable)

instance Writable a => Writable (WritableTuple a b) where
    write dst (WritableTuple (x, _)) = write dst x

writableTupleFst :: WritableTuple a b -> a
writableTupleFst = fst . unWritableTuple

writableTupleSnd :: WritableTuple a b -> b
writableTupleSnd = snd . unWritableTuple

writableTupleCompiler :: Compiler (a, b) (WritableTuple a b)
writableTupleCompiler = arr WritableTuple
