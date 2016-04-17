module Transform where
import Prim
import Sleigh
import qualified Graph as G

import qualified Data.Set as Set
import qualified Data.Map as Map

fuse :: Loop Var -> Loop Var -> Maybe (Loop Var)
fuse a b
 | loopExp a == loopExp b
 = Just
 $ Loop (loopBind la) (loopExp la)
 ( initialStage : loopStages la ++ loopStages rb)
 | otherwise
 = Nothing 
 where
  la = fmap ("fl_"++) a
  rb = fmap ("fr_"++) b

  initialStage
   = Stage
   [ LetUpdate (loopBind rb) (XVar $ BoundLet $ loopBind la) ]


anormal :: Loop Var -> Loop Var
anormal l
 = l { loopStages = fmap anormalStage $ loopStages l }
 where
  anormalStage (Stage stmts)
   = Stage $ concatMap anormalStmt stmts

  anormalStmt (Fold b z k)
   = let (prez, z') = breaky BoundFoldCurrent (b ++ "_zero") z
         (prek, k') = breaky BoundLet (b ++ "_kons") k
     in (fmap (\(b',x') -> Fold b' x' (XVar $ BoundFoldCurrent b')) prez)
     ++ (fmap (\(b',x') -> LetUpdate b' x') prek)
     ++ [Fold b z' k']

  anormalStmt (LetUpdate b k)
   = fmap (\(b',k') -> LetUpdate b' k')
   $ breako BoundLet b k

  breaky bAs b x
   = let (pre,x') = breaks bAs b x
     in  case reverse pre of
          [] -> ([], x')
          ((_,p'):pre')
           -> (reverse pre', p')

  breako bAs b x
   = case x of
      XPrim{} -> fst $ breaks bAs b x
      _       -> [(b, x)]

  breaks _ _ (XVar n)
   = ([], XVar n)
  breaks _ _ (XValue v)
   = ([], XValue v)
  breaks bAs b (XPrim p as)
   = let (pres,as') = unzip
                    $ map (\(a,n) -> breaks bAs (varmod b n) a)
                    $ zip as [0 :: Int ..]
     in  ( concat pres ++ [(b, XPrim p as')]
         , XVar $ bAs b)

  varmod b n = b ++ "_" ++ show n


-- | Split stages into strongly-connected components.
-- That is, so that each stage has one set of mutually recursive bindings.
--
-- Examples (square brackets means free variables)
--
-- stage {
--  a = [ b ]
--  b = [   ]
-- }
-- ==>
-- stage {
--  b = [   ]
-- }
-- stage {
--  a = [ b ]
-- }
--
-- stage {
--  a = [ b ]
--  b = [ a ]
-- }
-- ==>
-- stage {
--  a = [ b ]
--  b = [ a ]
-- }
--
splitStages :: Loop Var -> Loop Var
splitStages l
 = l { loopStages = concatMap G.stronglyConnectStage $ loopStages l }
   
