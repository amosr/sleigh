{-# LANGUAGE DeriveFunctor #-}
module Prim where

import qualified Data.Set as Set
import qualified Data.Map as Map

type Var = String

data Type = IntT
 deriving (Eq, Ord, Show)

data Prim
  = PrimAdd
  deriving (Eq, Ord, Show)

data Value = VInt Int
  deriving (Eq, Ord, Show)

-- | Very simple expressions
data Exp n
 = XVar (Bound n)
 | XValue Value
 | XPrim Prim [Exp n]
 deriving (Eq, Ord, Show, Functor)

-- | Reference to a bound variable
data Bound n
 = BoundFoldCurrent n
 | BoundFoldNew     n
 | BoundLet         n
  deriving (Eq, Ord, Show, Functor)

freeOfExp :: Ord n => Exp n -> Set.Set n
freeOfExp x
 = case x of
    XVar b      -> Set.singleton $ freeOfBound b
    XValue _    -> Set.empty
    XPrim _ xs  -> Set.unions $ fmap freeOfExp xs

freeOfBound :: Ord n => Bound n -> n
freeOfBound (BoundFoldCurrent n) = n
freeOfBound (BoundFoldNew     n) = n
freeOfBound (BoundLet         n) = n

