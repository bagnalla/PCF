-- This module defines the term evaluation function.

module Eval (
  eval
  ) where

import Ast
import Core (isValue, termSubst, boolOf)

-------------------------
-- Small-step evaluation.

-- One step of evaluation. Returns Nothing if no step can be taken.
eval1 :: Term info -> Maybe (Term info)
eval1 (TmApp fi t1 t2)
  | not (isValue t1) = do
      t1' <- eval1 t1
      return $ TmApp fi t1' t2
  | not (isValue t2) = do
      t2' <- eval1 t2
      return $ TmApp fi t1 t2'
eval1 (TmApp fi (TmAbs fi' x _ t1) t2) = return $ termSubst x t2 t1
  
eval1 (TmIf fi t1 t2 t3)
  | not (isValue t1) = do
      t1' <- eval1 t1
      return $ TmIf fi t1' t2 t3
  | otherwise        = if (boolOf t1) then return t2 else return t3

eval1 (TmFix fi (TmAbs fi' x ty t)) =
  return $ termSubst x (TmFix fi (TmAbs fi' x ty t)) t
eval1 (TmFix fi t) = do
  t' <- eval1 t
  return $ TmFix fi t'

eval1 (TmIszero fi (TmZero _)) = return $ TmTrue fi
eval1 (TmIszero fi t)
  | not (isValue t) = do
      t' <- eval1 t
      return $ TmIszero fi t'
  | otherwise = return $ TmFalse fi

eval1 (TmSucc fi t)
  | not (isValue t) = do
      t' <- eval1 t
      return $ TmSucc fi t'

eval1 (TmPred _ (TmZero fi))  = return $ TmZero fi
eval1 (TmPred _ (TmSucc _ t)) = return t
      
eval1 (TmPred fi t)
  | not (isValue t) = do
      t' <- eval1 t
      return $ TmPred fi t'

eval1 _ = Nothing

-- Reduce a term as much as possible.
eval :: Term info -> Term info
eval t =
  case do eval1 t of
    Just t' -> eval t'
    Nothing -> t
