-- This model defines the interpreter. It interprets each Command of a
-- program in sequence. Each command results in either an (Id, Term) pair
-- (in the case of CBind), or the result of evaluating a term to a normal
-- form (in the case of CEval). A list containing all of the command results
-- is returned. Typically, the last one will be thevalue of interest.

module Interp (
  interpCommand,
  interpProg
  ) where

import Control.Monad.State
import Ast
import Symtab (Symtab, Id, empty, add, fold)
import qualified Symtab (get)
import Core (termSubst, freeVars)
import Eval (eval)

type CommandResult info = Either (Id, Term info) (Term info)

type InterpState info = (Symtab (Term info), [CommandResult info])
initState = (empty, [])

-- Interpret a program by interpreting its commands in sequence.
interpProg :: Prog info -> [CommandResult info]
interpProg = (`evalState` initState) . interpCommands . prog_of

-- Use a state monad to interpret commands in sequence. The state is
-- a global environment mapping [Id]s to [Term]s and it is updated by
-- CBind commands.
interpCommands :: [Command info] -> State (InterpState info)
                    [CommandResult info]
interpCommands [] = do
  (_, results) <- get
  return results
interpCommands (c:cs) = do
  (env, results) <- get
  let res = interpCommand env c
  put $ case res of
    Left (x, t) -> (add x (substEnv env t) env, results ++ [res])
    Right t -> (env, results ++ [res])
  interpCommands cs

-- Interpret a single command by either returning an [(Id, Term)] pair
-- or by evaluating the term it contains and returning its normal form.
interpCommand :: Symtab (Term info) -> Command info ->
                   Either (Id, Term info) (Term info)
interpCommand _ (CBind fi id t) = Left (id, t)
interpCommand env (CEval fi t) =
  Right (eval (fold (\acc k v -> termSubst k v acc) t env))

-- Substitute all Ids bound in the global environment into the given
-- term (avoiding capture of course).
substEnv :: Symtab (Term info) -> Term info -> Term info
substEnv env t =
    foldl (\acc id ->
            case Symtab.get id env of
              Just v  -> termSubst id v acc
              Nothing -> error "free variable not bound in environment")
    t (freeVars t)
