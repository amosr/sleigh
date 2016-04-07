module Print where
import Prim
import Sleigh

import Data.List (intercalate)

printPrim :: Prim -> String
printPrim p
 = case p of
    PrimAdd -> "add_int"

printBound :: Bound Var -> String
printBound b
 = case b of
    BoundFoldCurrent  v -> v ++ "_fold"
    BoundFoldNew      v -> v ++ "_fold_new"
    BoundLet          v -> v ++ "_let"

printType :: Type -> String
printType t
 = case t of
    IntT -> "int"

printValue :: Value -> String
printValue v
 = case v of
    VInt i -> show i

printExp :: Exp' -> String
printExp x
 = case x of
    XVar n   -> printBound n
    XValue v -> printValue v
    XPrim p as
     -> printPrim p
     ++ "("
     ++ intercalate ", " (map printExp as)
     ++ ")"

printLoop :: Loop Var -> String
printLoop l
 = unlines
 ( loopInits
 ++ ["for (" ++ loopIteration ++ ") {"]
 ++ fmap ("  " ++) loopBody
 ++ ["}"] )
 where
  loopIteration
   = let b  = loopBind l
         v  = bindVar  b
         v' = BoundLet v
         x  = loopExp  l
     in printBindAs BoundLet b "0"
        ++ " "
        ++ printBound v' ++ "++; "
        ++ printBound v' ++ " != " ++ printExp x

  loopInits
   = concatMap printStageInits
   $ loopStages l

  loopBody
   =  concatMap printStageBody    (loopStages l)
   ++ ["// Update all fold accumulators with new values"]
   ++ concatMap printStageBodyEnd (loopStages l)


printStageInits :: Stage Var -> [String]
printStageInits (Stage sts)
 = concatMap pInit sts
 where
  pInit (Fold b z _)
    =[printBindAs BoundFoldCurrent b $ printExp z]
  pInit (LetUpdate{})
    = []

printStageBody :: Stage Var -> [String]
printStageBody (Stage sts)
 = concatMap pInit sts ++ [""]
 where
  pInit (Fold b _ k)
    =[printBindAs BoundFoldNew b $ printExp k]
  pInit (LetUpdate b k)
    =[printBindAs BoundLet     b $ printExp k]

printStageBodyEnd :: Stage Var -> [String]
printStageBodyEnd (Stage sts)
 = concatMap pInit sts
 where
  pInit (Fold b _ k)
    =[printBound (BoundFoldCurrent $ bindVar b) ++ " = " ++ printBound (BoundFoldNew $ bindVar b) ++ ";"]
  pInit (LetUpdate b k)
    =[]


printBindAs :: (Var -> Bound Var) -> Bind Var -> String -> String
printBindAs f (Bind v t) init
 = printType t ++ " " ++ printBound (f v) ++ " = " ++ init ++ ";"
