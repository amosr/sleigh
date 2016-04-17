module Example where
import Prim
import Sleigh
import Check
import PrintS
import Transform
import Eval

example :: Loop Var
example
 = Loop "loop" (i 5)
 [ Stage
  [ Fold "count" (i 0) (bfc "count" `plus` i 1) ]
 , Stage
  [ Fold "sum" (i 0) (bfc "sum" `plus` bl "loop")
  , LetUpdate "sumsum" (bfc "count" `plus` bfc "sum" `plus` bfn "sum")]
 , Stage
  [ Fold "a" (i 0) (bfc "b" `plus` i 1)
  , Fold "b" (i 1) (bfc "a" `plus` i 1)
  ]
 ]

 where
  bfc = XVar . BoundFoldCurrent
  bfn = XVar . BoundFoldNew
  bl  = XVar . BoundLet
  plus a b = XPrim PrimAdd [a,b]
  i   = XValue . VInt
