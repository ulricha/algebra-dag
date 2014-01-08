-- Provides a basic toolset for materialization functions.
module Database.Algebra.SQL.Materialization where

import Database.Algebra.SQL.Tile (TileTree, DependencyList)
import Database.Algebra.SQL.Query (Query)

-- The type of materialization function. The result consists of:
--
--      * The queries which need to be executed first
--
--      * The queries produced from the root nodes
--
type MatFun = (([TileTree], DependencyList) -> ([Query], [Query]))

