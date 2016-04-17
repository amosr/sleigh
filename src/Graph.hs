module Graph where
import Sleigh

import qualified Data.Set as Set
import qualified Data.Graph as Graph

stronglyConnectStage :: Ord n => Stage n -> [Stage n]
stronglyConnectStage (Stage ss)
 = let gs = fmap graphNodeOfStatement ss
       cs = Graph.stronglyConnComp gs
   in  fmap (Stage . Graph.flattenSCC) cs


graphNodeOfStatement :: Ord n => Statement n -> (Statement n, n, [n])
graphNodeOfStatement s
 = (s, bindOfStatement s, Set.toList $ freeOfStatement s)

