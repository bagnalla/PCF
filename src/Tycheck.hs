-- This module contains the typechecker. It is a function from untyped ASTs
-- that are annotated with line/column number information to ASTs with
-- type annotations.

module Tycheck (
  TyInfo,
  tycheckProg,
  runTycheck
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

import Ast
import Core (info_of_term, info_of_command)
import Parser
import Symtab (Symtab, empty, add, get)

data TyInfo =
  TyInfo { ty_of :: Type }
  deriving (Show)

mkInfo ty = TyInfo { ty_of = ty }

ty_of_term :: Term TyInfo -> Type
ty_of_term = ty_of . info_of_term

ty_of_command :: Command TyInfo -> Type
ty_of_command = ty_of . info_of_command

---------------
-- Typechecker

-- Following the example from "Monad Transformers Step by Step"
-- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf

type Context = Symtab Type

type Tycheck a = ReaderT Context (ExceptT String Identity) a

runTycheck :: Tycheck a -> Either String a
runTycheck t = runIdentity (runExceptT (runReaderT t empty))

assertTy :: Show info => Term info -> Type -> Tycheck (Term TyInfo)
assertTy t ty = do
  t' <- tycheckTerm t
  if ty_of_term t' /= ty then
    throwError $ "Type error: " ++ show (info_of_term t)
  else
    return $ t'

tycheckTerm :: Show info => Term info -> Tycheck (Term TyInfo)
tycheckTerm (TmVar fi id) = do
  -- 'asks' takes a function mapping the context to a value
  ty <- asks (get id)
  case ty of
    Just ty' -> return $ TmVar (mkInfo ty') id
    Nothing  -> throwError $ "Type error: " ++ show fi

tycheckTerm (TmAbs fi id ty t) = do
  t' <- local (add id ty) (tycheckTerm t)
  let ty' = ty_of_term t'
  return $ TmAbs (mkInfo (TyArrow ty ty')) id ty t'

tycheckTerm (TmApp fi t1 t2) = do
  t1' <- tycheckTerm t1
  t2' <- tycheckTerm t2
  let ty1 = ty_of_term t1'
  let ty2 = ty_of_term t2'
  case ty1 of
    TyArrow ty1' ty2' ->
      if ty1' /= ty2 then
        throwError $ "Type error : " ++ show fi
      else
        return $ TmApp (mkInfo ty2') t1' t2'

tycheckTerm (TmTrue fi) =
  return $ TmTrue (mkInfo TyBool)

tycheckTerm (TmFalse fi) =
  return $ TmFalse (mkInfo TyBool)

tycheckTerm (TmIf fi t1 t2 t3) = do
  t1' <- assertTy t1 TyBool
  t2' <- tycheckTerm t2
  let ty2 = ty_of_term t2'
  t3' <- assertTy t3 ty2
  return $ TmIf (mkInfo ty2) t1' t2' t3'

tycheckTerm (TmZero fi) =
  return $ TmZero (mkInfo TyNat)

tycheckTerm (TmSucc fi t) = do
  t' <- assertTy t TyNat
  return $ TmSucc (mkInfo TyNat) t'
  
tycheckTerm (TmPred fi t) = do
  t' <- assertTy t TyNat
  return $ TmPred (mkInfo TyNat) t'
  
tycheckTerm (TmIszero fi t) = do
  t' <- assertTy t TyNat
  return $ TmIszero (mkInfo TyBool) t'

tycheckTerm (TmFix fi t) = do
  t' <- tycheckTerm t
  let ty = ty_of_term t'
  case ty of
    TyArrow ty1' ty2' ->
      if ty1' == ty2' then
        return $ TmFix (mkInfo ty1') t'
      else
        throwError $ "Type error: " ++ show (info_of_term t)
    _ ->
      throwError $ "Type error: " ++ show (info_of_term t)

tycheckCommand :: Show info => Command info -> Tycheck (Command TyInfo)
tycheckCommand (CBind fi id t) = do
  t' <- tycheckTerm t
  return $ CBind (mkInfo (ty_of_term t')) id t'
tycheckCommand (CEval fi t) = do
  t' <- tycheckTerm t
  return $ CEval (mkInfo (ty_of_term t')) t'

tycheckCommands :: Show info => [Command info] -> Tycheck [Command TyInfo]
tycheckCommands [] = return []
tycheckCommands (c:cs) = do
  c' <- tycheckCommand c
  let ty = ty_of_command c'
  case c' of
    CBind _ id _ -> do
      cs' <- local (add id ty) (tycheckCommands cs)
      return $ c' : cs'
    _ -> do
      cs' <- tycheckCommands cs
      return $ c' : cs'

tycheckProg :: Show info => Prog info -> Tycheck (Prog TyInfo)
tycheckProg p = do
  cs <- tycheckCommands (prog_of p)
  return $ Prog { pinfo_of = mkInfo TyBool, prog_of = cs }
