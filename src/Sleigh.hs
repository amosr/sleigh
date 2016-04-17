{-# LANGUAGE DeriveFunctor #-}
module Sleigh where
import Prim

import qualified Data.Set as Set

-- | An entire loop
data Loop n
 = Loop {
   -- | Loop iterator: name to give whatever we loop over.
   -- This is bound as a LetUpdate and so cannot be referenced from initialisers.
   loopBind   :: n
   -- | Whatever we loop over
 , loopExp    :: Exp n
   -- | What to perform
 , loopStages :: [Stage n] }
 deriving (Show, Functor)

-- | Each stage allows a set of mutually recursive statement bindings.
-- The order of the statements does not affect evaluation,
-- but it can affect whether the program typechecks.
-- That is: if two different orderings both typecheck, they are equivalent.
data Stage n
 = Stage {
   stageStatements :: [Statement n] }
 deriving (Show, Functor)

-- | A statement binding inside a stage.
-- There are two kinds of statements, the main difference being where their bindings are available,
-- and which bindings are available in their definition.
-- Folds define an initialiser expression and an update expression to perform on each iteration.
-- LetUpdate is the same as an update expression and can refer to the loop iterator and other LetUpdates.
--
-- These are split up because otherwise one cannot cleanly create a fold for something like
-- "loop iterator * 2": since the loop iterator is only available during iterations, a fold
-- would need a dummy expression (0, for example) for its initialiser.
data Statement n
 -- | Folds.
 -- First expression is the initialiser, and can only refer to previously bound folds.
 -- The previously bound folds should only be mentioned as BoundFoldCurrent;
 -- really, both New and Current mean the same thing here, but alpha equivalence is simplified
 -- by having only one name for each fold.
 -- The initialiser cannot refer to the loop iterator, nor any LetUpdates.
 --
 -- The second expression is the update. It can refer to the new value (BoundFoldNew) of any
 -- previous folds, as well as the start-of-iteration value (BoundFoldCurrent) of folds in
 -- this stage, as well as previous folds.
 -- It can also refer to the loop iterator and previous LetUpdates.
 = Fold       n (Exp n) (Exp n)
 -- | Update let bindings.
 -- This can refer to the same things as a fold update: new and start-of-iteration values of folds,
 -- the current loop iterator, and previous LetUpdates.
 | LetUpdate  n (Exp n)
 deriving (Show, Functor)


type Exp' = Exp Var

bindOfStatement :: Statement n -> n
bindOfStatement (Fold n _ _)     = n
bindOfStatement (LetUpdate n _)  = n

freeOfStatement :: Ord n => Statement n -> Set.Set n
freeOfStatement (Fold _ z k)
 = freeOfExp k `Set.union` freeOfExp z
freeOfStatement (LetUpdate _ k)
 = freeOfExp k

