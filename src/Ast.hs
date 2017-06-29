-- This module defines the internal language syntax.

{-# LANGUAGE DeriveFunctor #-}
module Ast (
  Type(..),
  Term(..),
  Command(..),
  Prog(..)
  ) where

import Data.List (intercalate)
import Context (Id(..))

-------
-- Types

data Type =
  TyBool
  | TyNat
  | TyArrow Type Type
  deriving (Eq)

-------
-- Terms

-- Terms are parameterized by the type of extra information (their type,
-- location in source file, etc)

data Term info =
  TmVar info Id
  | TmAbs info Id Type (Term info)
  | TmApp info (Term info) (Term info)
  | TmTrue info
  | TmFalse info
  | TmIf info (Term info) (Term info) (Term info)
  | TmZero info
  | TmSucc info (Term info)
  | TmPred info (Term info)
  | TmIszero info (Term info)
  | TmFix info (Term info)
  deriving (Eq, Functor)

-----------
-- Commands

-- A command either binds a term to an Id in the global context, or
-- evaluates a term to a normal form.

data Command info =
  CBind info Id (Term info)
  | CEval info (Term info)
  deriving (Eq, Functor)

----------
-- Program

data Prog info =
  Prog { pinfo_of :: info,
         prog_of  :: [Command info] }
  deriving (Eq, Functor)

----------------------
-- Typeclass Instances

instance Show Type where
  show TyBool = "bool"
  show TyNat = "nat"
  show (TyArrow t1 t2) = show t1 ++ "->" ++ show t2

-- instance Show (Term info) where
--   show (TmVar _ id)      = show id
--   show (TmAbs _ id ty t) = "(\\" ++ show id ++ ":" ++ show ty ++
--                              " => " ++ show t ++ ")"
--   show (TmApp _ t1 t2)   = show t1 ++ " " ++ show t2
--   show (TmTrue _)        = "true"
--   show (TmFalse _)       = "false"
--   show (TmIf _ t1 t2 t3) = "if " ++ show t1 ++ " then " ++
--                              show t2 ++ " else " ++ show t3
--   show (TmZero _)        = "0"
--   show (TmSucc _ t)      = "(succ " ++ show t ++ ")"
--   show (TmPred _ t)      = "(pred " ++ show t ++ ")"
--   show (TmIszero _ t)    = "(iszero " ++ show t ++ ")"
--   show (TmFix _ t)       = "(fix " ++ show t ++ ")"

instance Show (Term info) where
  show (TmVar _ id)      = show id
  show (TmAbs _ id ty t) = "(TmAbs " ++ show id ++ " " ++ show ty ++
                             " " ++ show t ++ ")"
  show (TmApp _ t1 t2)   = "(TmApp " ++ show t1 ++ " " ++ show t2 ++ ")"
  show (TmTrue _)        = "true"
  show (TmFalse _)       = "false"
  show (TmIf _ t1 t2 t3) = "(TmIf " ++ show t1 ++ " " ++ show t2 ++
                             " " ++ show t3 ++ ")"
  show (TmZero _)        = "0"
  show (TmSucc _ t)      = "(succ " ++ show t ++ ")"
  show (TmPred _ t)      = "(pred " ++ show t ++ ")"
  show (TmIszero _ t)    = "(iszero " ++ show t ++ ")"
  show (TmFix _ t)       = "(fix " ++ show t ++ ")"

  
instance Show (Command info) where
  show (CBind _ (Id s) t) = s ++ " = " ++ show t
  show (CEval _ t)        = "eval " ++ show t

instance Show (Prog info) where
  show (Prog { prog_of = p }) = intercalate ", " (map show p)
