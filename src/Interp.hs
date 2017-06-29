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
import Context (Context, Id, empty, add, fold)
import Core (termSubst)
import Eval (eval)

type CommandResult info = Either (Id, Term info) (Term info)

type InterpState info = (Context (Term info), [CommandResult info])
initState = (empty, [])

-- Interpret a program by interpreting its commands in sequence.
interpProg :: Prog info -> [CommandResult info]
interpProg p =
  evalState (do interpCommands (prog_of p)) initState

-- Use a state monad to interpret commands in sequence. The state is
-- a global context mapping [Id]s to [Term]s and it is updated by CBind
-- commands.
interpCommands :: [Command info] -> State (InterpState info)
                    [CommandResult info]
interpCommands [] = do
  (_, results) <- get
  return results
interpCommands (c:cs) = do
  (ctx, results) <- get
  let res = interpCommand ctx c
  put $ case res of
    Left (x, t) -> (add x (substContext ctx t) ctx, results ++ [res])
    Right t -> (ctx, results ++ [res])
  interpCommands cs

-- Interpret a single command by either returning an [(Id, Term)] pair
-- or by evaluating the term it contains and returning its normal form.
interpCommand :: Context (Term info) -> Command info ->
                   Either (Id, Term info) (Term info)
interpCommand _ (CBind fi id t) = Left (id, t)
interpCommand ctx (CEval fi t) =
  Right (eval (fold (\acc k v -> termSubst k v acc) t ctx))

-- Substitute all Ids bound in the context into the given term
-- (avoiding capture of course).
substContext :: Context (Term info) -> Term info -> Term info
substContext ctx t = fold (\acc k v -> termSubst k v acc) t ctx
