{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}
module Prim where

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
 = XVar n
 | XValue Value
 | XPrim Prim [Exp n]
 deriving (Eq, Ord, Show, Functor)

checkP :: Prim -> [Type] -> Maybe Type
checkP p ts
 = case p of
    PrimAdd
     | [IntT,IntT] <- ts
     -> Just IntT
     | otherwise
     -> Nothing

checkV :: Value -> Maybe Type
checkV v
 = case v of
    VInt _ -> Just IntT

checkX :: Ord n => Map.Map n Type -> Exp n -> Maybe Type
checkX env x
 = case x of
    XVar n   -> Map.lookup n env
    XValue v -> checkV v
    XPrim p xs
     -> do ts <- mapM (checkX env) xs
           checkP p ts

