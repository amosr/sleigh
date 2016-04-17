module Check where
import Prim
import Sleigh

import qualified Data.Map as Map
import Control.Monad (foldM, when)

type Env n = Map.Map (Bound n) Type

data CheckError n
 = CheckErrorNoSuchVar (Bound n)
 | CheckErrorLoopVarNotInt Type
 | CheckErrorFoldPartsDifferentType (Statement n) Type Type
 | CheckErrorImpossibleStatement (Statement n)
 | CheckErrorShadowing n
 | CheckErrorPrimitiveBadArgs Prim [(Exp n, Type)]
 deriving (Show)

checkLoop :: Ord n => Loop n -> Either (CheckError n) (Env n)
checkLoop l
 = do t <- checkX Map.empty (loopExp l)
      when (t /= IntT) (Left $ CheckErrorLoopVarNotInt t)

      -- Initialiser environment
      let ie0 = Map.empty
      -- Update environment
      let ue0 = Map.singleton (BoundLet $ loopBind l) t

      (ie',_) <- foldM checkStage (ie0, ue0) (loopStages l)
      -- The result only includes things in the initial environment
      -- That is, it excludes LetUpdates etc.
      return ie'

checkStage :: Ord n => (Env n,Env n) -> Stage n -> Either (CheckError n) (Env n,Env n)
checkStage (ie0,ue0) (Stage bs)
 = do (ie1,ue1) <- foldM insertPreStmt (ie0,ue0) bs
      ue2       <- foldM checkStmt ue1 bs
      return (ie1,ue2)
 where
  insertPreStmt (ie,ue) (Fold v z _)
   = do checkNoShadowing v ie
        checkNoShadowing v ue
        tz <- checkX ie z
        let ins = Map.insert (BoundFoldCurrent v) tz
        return (ins ie, ins ue)
  insertPreStmt (ie,ue) LetUpdate{}
   =    return (ie,ue)

checkStmt :: Ord n => Env n -> Statement n -> Either (CheckError n) (Env n)
checkStmt ue0 s
 = case s of
   Fold v z k 
    | Just tz <- Map.lookup (BoundFoldCurrent v) ue0
    -> do tk  <- checkX ue0 k
          when (tk /= tz) (Left $ CheckErrorFoldPartsDifferentType s tz tk)
          -- ue has already been checked for shadowing,
          -- and already contains BoundFoldCurrent.
          -- Next stmt can refer to BoundFoldNew though
          return $ Map.insert (BoundFoldNew v) tk ue0
    | otherwise
    -> Left $ CheckErrorImpossibleStatement s

   LetUpdate v k
    -> do tk  <- checkX ue0 k
          checkNoShadowing v ue0
          return $ Map.insert (BoundLet v) tk ue0


checkNoShadowing :: Ord n => n -> Env n -> Either (CheckError n) ()
checkNoShadowing v e
 = when (anybound [BoundFoldCurrent v, BoundFoldNew v, BoundLet v])
 $ Left $ CheckErrorShadowing v
 where
  anybound
   = any (flip Map.member e)


checkP :: Prim -> [(Exp n, Type)] -> Either (CheckError n) Type
checkP p xts
 = case p of
    PrimAdd
     | [IntT,IntT] <- ts
     -> return IntT
     | otherwise
     -> Left $ CheckErrorPrimitiveBadArgs p xts
 where
  ts = fmap snd xts

checkV :: Value -> Either (CheckError n) Type
checkV v
 = case v of
    VInt _ -> return IntT

checkX :: Ord n => Env n -> Exp n -> Either (CheckError n) Type
checkX env x
 = case x of
    XVar n
     | Just t <- Map.lookup n env
     -> return t
     | otherwise
     -> Left $ CheckErrorNoSuchVar n
    XValue v -> checkV v
    XPrim p xs
     -> do ts <- mapM (checkX env) xs
           checkP p (xs `zip` ts)

