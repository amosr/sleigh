module PrintS where
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
    BoundFoldCurrent  v -> v ++ ""
    BoundFoldNew      v -> v ++ "<new>"
    BoundLet          v -> v ++ ""

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
 (  ["loop (" ++ loopIteration ++ ") {"]
 ++ fmap ("  " ++) loopBody
 ++ ["}"] )
 where
  loopIteration
   = let b  = loopBind l
         x  = loopExp  l
     in printBind b ++ " <- 0.."  ++ printExp x

  loopBody
   =  concatMap printStage (loopStages l)


printStage :: Stage Var -> [String]
printStage (Stage sts)
 =  ["stage {"]
 ++ concatMap pp sts
 ++ ["}"]
 where
  pp (Fold b z k)
    = [ "  fold " ++ printBind b
      , "        = " ++ printExp z
      , "     then " ++ printExp k ++ ";" ]
  pp (LetUpdate b k)
    = [ "  let  " ++ printBind b
      , "        = " ++ printExp k ++ ";" ]

printBind :: Var -> String
printBind v
 = v
