module Database.Gerippe.Utils where

import           Data.Map (Map)
import qualified Data.Map as Map

collectSnd :: Ord k => [(k,v)] -> Map k [v]
collectSnd = foldl accum Map.empty
  where accum m (k,v) = Map.insertWith (++) k [v] m
