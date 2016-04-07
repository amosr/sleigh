module Check where
import Prim
import Sleigh

import qualified Data.Map as Map
import Control.Monad (foldM, when)

type Env n = Map.Map (Bound n) Type

checkLoop :: Ord n => Loop n -> Maybe (Env n)
checkLoop l
 = do t <- checkX Map.empty (loopExp l)
      when ((bindType $ loopBind l) /= t) Nothing

      -- Initialiser environment
      let ie0 = Map.empty
      -- Update environment
      let ue0 = Map.singleton (BoundLet $ bindVar $ loopBind l) t

      (ie',_) <- foldM checkStage (ie0, ue0) (loopStages l)
      -- The result only includes things in the initial environment
      -- That is, it excludes LetUpdates etc.
      return ie'

checkStage :: Ord n => (Env n,Env n) -> Stage n -> Maybe (Env n,Env n)
checkStage (ie0,ue0) (Stage bs)
 = do ue1 <- foldM insertPreStmt ue0 bs
      foldM checkStmt (ie0,ue1) bs
 where
  insertPreStmt ue (Fold (Bind v t) _ _)
   = do checkNoShadowing v ue
        return $ Map.insert (BoundFoldCurrent v) t ue
  insertPreStmt ue LetUpdate{}
   =    return ue

checkStmt :: Ord n => (Env n,Env n) -> Statement n -> Maybe (Env n,Env n)
checkStmt (ie0,ue0) s
 = case s of
   Fold (Bind v t) z k 
    -> do tz  <- checkX ie0 z
          tk  <- checkX ue0 k
          when (t /= tz || tz /= tk) Nothing
          checkNoShadowing v ie0
          let ie1 = Map.insert (BoundFoldCurrent v) t ie0
          -- ue has already been checked for shadowing,
          -- and already contains BoundFoldCurrent.
          -- Next stmt can refer to BoundFoldNew though
          let ue1 = Map.insert (BoundFoldNew v) t ue0
          return (ie1, ue1)

   LetUpdate (Bind v t) k
    -> do tk  <- checkX ue0 k
          when (t /= tk) Nothing
          checkNoShadowing v ue0
          let ue1 = Map.insert (BoundLet v) t ue0
          return (ie0, ue1)


checkNoShadowing :: Ord n => n -> Env n -> Maybe ()
checkNoShadowing v e
 = when (anybound [BoundFoldCurrent v, BoundFoldNew v, BoundLet v]) Nothing
 where
  anybound
   = any (flip Map.member e)


