-- This module contains the typechecker. It is a function from untyped ASTs
-- that are annotated with line/column number information to ASTs with
-- type annotations.

module Tycheck (
  TyInfo,
  tycheckProg
  ) where

import Ast
import Core (info_of_term, info_of_command)
import Parser
import Context (Context, empty, add, get)

data TyInfo =
  TyInfo { ty_of :: Type }
  deriving (Show)

mkInfo ty = TyInfo { ty_of = ty }

ty_of_term :: Term TyInfo -> Type
ty_of_term = ty_of . info_of_term

ty_of_command :: Command TyInfo -> Type
ty_of_command = ty_of . info_of_command

tycheckTerm :: Show info => Context Type -> Term info ->
                 Either String (Term TyInfo)
tycheckTerm ctx (TmVar fi id) = do
  let ty = get id ctx
  case ty of
    Just ty' -> Right $ TmVar (mkInfo ty') id
    Nothing  -> Left $ "Type error: " ++ show fi

tycheckTerm ctx (TmAbs fi id ty t) = do
  t' <- tycheckTerm (add id ty ctx) t
  let ty' = ty_of_term t'
  return $ TmAbs (mkInfo (TyArrow ty ty')) id ty t'

tycheckTerm ctx (TmApp fi t1 t2) = do
  t1' <- tycheckTerm ctx t1
  t2' <- tycheckTerm ctx t2
  let ty1 = ty_of_term t1'
  let ty2 = ty_of_term t2'
  case ty1 of
    TyArrow ty1' ty2' ->
      if ty1' /= ty2 then
        Left $ "Type error : " ++ show fi
      else
        return $ TmApp (mkInfo ty2') t1' t2'

tycheckTerm ctx (TmTrue fi) = do
  return $ TmTrue (mkInfo TyBool)

tycheckTerm ctx (TmFalse fi) = do
  return $ TmFalse (mkInfo TyBool)

tycheckTerm ctx (TmIf fi t1 t2 t3) = do
  t1' <- assertTy ctx t1 TyBool
  t2' <- tycheckTerm ctx t2
  let ty2 = ty_of_term t2'
  t3' <- assertTy ctx t3 ty2
  return $ TmIf (mkInfo ty2) t1' t2' t3'

tycheckTerm ctx (TmZero fi) = do
  return $ TmZero (mkInfo TyNat)

tycheckTerm ctx (TmSucc fi t) = do
  t' <- assertTy ctx t TyNat
  return $ TmSucc (mkInfo TyNat) t'
  
tycheckTerm ctx (TmPred fi t) = do
  t' <- assertTy ctx t TyNat
  return $ TmPred (mkInfo TyNat) t'
  
tycheckTerm ctx (TmIszero fi t) = do
  t' <- assertTy ctx t TyNat
  return $ TmIszero (mkInfo TyBool) t'

tycheckTerm ctx (TmFix fi t) = do
  t' <- tycheckTerm ctx t
  let ty = ty_of_term t'
  case ty of
    TyArrow ty1' ty2' ->
      if ty1' == ty2' then
        return $ TmFix (mkInfo ty1') t'
      else
        Left $ "Type error: " ++ show (info_of_term t)
    _ ->
      Left $ "Type error: " ++ show (info_of_term t)
  
assertTy :: Show info => Context Type -> Term info -> Type ->
              Either String (Term TyInfo)
assertTy ctx t ty = do
  t' <- tycheckTerm ctx t
  if ty_of_term t' /= ty then
    Left $ "Type error: " ++ show (info_of_term t)
  else
    Right $ t'

tycheckCommand :: Show info => Context Type -> Command info ->
                    Either String (Command TyInfo)
tycheckCommand ctx (CBind fi id t) = do
  t' <- tycheckTerm ctx t
  return $ CBind (mkInfo (ty_of_term t')) id t'
tycheckCommand ctx (CEval fi t) = do
  t' <- tycheckTerm ctx t
  return $ CEval (mkInfo (ty_of_term t')) t'

tycheckCommands :: Show info => Context Type -> [Command info] ->
                     Either String [Command TyInfo]
tycheckCommands ctx [] = return []
tycheckCommands ctx (c:cs) = do
  c' <- tycheckCommand ctx c
  let ty = ty_of_command c'
  case c' of
    CBind _ id _ -> do
      cs' <- tycheckCommands (add id ty ctx) cs
      return $ c' : cs'
    _ -> do
      cs' <- tycheckCommands ctx cs
      return $ c' : cs'

tycheckProg :: Show info => Prog info -> Either String (Prog TyInfo)
tycheckProg p = do
  cs <- tycheckCommands empty (prog_of p)
  return $ Prog { pinfo_of = mkInfo TyBool, prog_of = cs }
