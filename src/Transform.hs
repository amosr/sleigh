module Transform where
import Prim
import Sleigh

fuse :: Loop Var -> Loop Var -> Maybe (Loop Var)
fuse a b
 | loopExp a == loopExp b
 = Just
 $ Loop (loopBind a) (loopExp a)
 ( initialStage : loopStages la ++ loopStages rb)
 | otherwise
 = Nothing 
 where
  la = fmap ("fuse_l$"++) a
  rb = fmap ("fuse_r$"++) b

  initialStage
   = Stage
   [ LetUpdate (loopBind rb) (XVar $ BoundLet $ bindVar $ loopBind la) ]

{-
anormal :: Loop Var -> Loop Var
anormal l
 = l { loopStages = fmap anormalStage $ loopStages l }
 where
  anormalStage (Stage stmts)
   = Stage $ concatMap anormalStmt stmts

  anormalStmt (Fold b z k)
   = 

  anormalStmt (LetUpdate b k)
   = pull 


-}
