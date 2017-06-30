-- Maybe this module should be called Symtab or something. It defines a
-- type for identifiers along with an ADT for mapping them to values.

module Symtab (
  Id(..),
  Symtab,
  empty,
  add,
  get,
  fold
  ) where

-- Use Haskell's map data structure
import qualified Data.Map.Strict as Map

-- an Id is just a String
newtype Id = Id String
  deriving (Eq, Ord)

-- A Symtab maps Ids to values of some type
type Symtab a = Map.Map Id a

-- The empty Symtab
empty = Map.empty 

-- Add a binding to a Symtab
add k = Map.insert k

-- Get the value bound to an Id
get k = Map.lookup k

-- Fold over all key/value pairs
fold :: (a -> Id -> b -> a) -> a -> Symtab b -> a
fold f = Map.foldlWithKey f

----------------------
-- Typeclass instances

instance Show Id where
  show (Id s) = s
