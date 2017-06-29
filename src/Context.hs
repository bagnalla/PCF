-- Maybe this module should be called Symtab or something. It defines a
-- type for identifiers along with an ADT for mapping them to values.

module Context (
  Id(..),
  Context,
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

-- A Context maps Ids to values of some type
type Context a = Map.Map Id a

-- The empty context
empty = Map.empty 

-- Add a binding to a context
add k = Map.insert k

-- Get the value bound to an Id
get k = Map.lookup k

-- Fold over all key/value pairs
fold :: (a -> Id -> b -> a) -> a -> Context b -> a
fold f = Map.foldlWithKey f

----------------------
-- Typeclass instances

instance Show Id where
  show (Id s) = s
