module Example where
import Prim
import Sleigh
import Check
import Print
import Transform

example :: Loop Var
example
 = Loop (b "loop") (i 100)
 [ Stage
  [ Fold (b "count") (i 0) (bfc "count" `plus` i 1) ]
 , Stage
  [ Fold (b "sum") (i 0) (bfc "sum" `plus` bl "loop")
  , LetUpdate (b "sumsum") (bfc "count" `plus` bfc "sum" `plus` bfn "sum")]
 ]

 where
  b v = Bind v IntT
  bfc = XVar . BoundFoldCurrent
  bfn = XVar . BoundFoldNew
  bl  = XVar . BoundLet
  plus a b = XPrim PrimAdd [a,b]
  i   = XValue . VInt
