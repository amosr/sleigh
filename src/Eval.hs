{-# LANGUAGE ScopedTypeVariables #-}
module Eval where

import Prim
import Sleigh

import qualified Data.Map as Map
import Control.Monad (foldM)


type Heap n = Map.Map (Bound n) Value

data EvalError n
 = EvalErrorNoSuchVariable (Bound n)
 | EvalErrorPrimitiveBadArgs (Exp n) Prim [Value] 
 deriving Show


evalLoop :: forall n. Ord n => Loop n -> Either (EvalError n) [Map.Map n Value]
evalLoop l
 = do v  <- evalExp Map.empty $ loopExp l
      hz <- foldM goZ Map.empty allS
      case v of
       VInt i -> goLoop hz 0 i
 where
  goLoop :: Heap n -> Int -> Int -> Either (EvalError n) [Map.Map n Value]
  goLoop h ix to
   | ix >= to
   = return [retHeap h]
   | otherwise
   = do let h' = Map.insert (BoundLet $ loopBind l) (VInt ix) h
        h'' <- foldM goK h' allS
        rs <- goLoop (updateHeap h'') (ix+1) to
        return (retHeap h : rs)

  allS = concatMap stageStatements
       $ loopStages l

  retHeap :: Heap n -> Map.Map n Value
  retHeap
   = Map.fromList
   . fmap (\(k,v) -> (freeOfBound k, v))
   . Map.toList

  updateHeap :: Heap n -> Heap n
  updateHeap
   = Map.fromList
   . concatMap
      (\(k,v) ->
       case k of
        BoundFoldCurrent _ -> []
        BoundFoldNew n     -> [(BoundFoldCurrent n, v)]
        BoundLet n         -> [(BoundLet n, v)])
   . Map.toList

  goZ h s
   = case s of
      Fold n z _
       -> do v <- evalExp h z
             return $ Map.insert (BoundFoldCurrent n) v h
      LetUpdate{}
       -> return h

  goK h s
   = case s of
      Fold n _ k
       -> do v <- evalExp h k
             return $ Map.insert (BoundFoldNew n) v h
      LetUpdate n k
       -> do v <- evalExp h k
             return $ Map.insert (BoundLet n) v h



evalExp :: Ord n => Heap n -> Exp n -> Either (EvalError n) Value
evalExp h x
 = case x of
    XVar n
     | Just v <- Map.lookup n h
     -> return v
     | otherwise
     -> Left $ EvalErrorNoSuchVariable n
    XValue v
     -> return v
    XPrim p xs
     -> mapM (evalExp h) xs >>= evalP p

 where
  evalP PrimAdd [VInt a, VInt b]
   = return $ VInt (a + b)
  evalP p vs
   = Left $ EvalErrorPrimitiveBadArgs x p vs

