-- This module provides utility functions for working with inner language
-- (mostly related to evaluation of terms).

module Core (
  isValue,
  termSubst,
  boolOf,
  info_of_term,
  info_of_command
  ) where

import Ast
import Context (Id)

isValue :: Term info -> Bool
isValue (TmTrue _)      = True
isValue (TmFalse _)     = True
isValue (TmAbs _ _ _ _) = True
isValue tm              = isNumericValue tm

isNumericValue :: Term info -> Bool
isNumericValue (TmZero _)   = True
isNumericValue (TmSucc _ t) = isNumericValue t
isNumericValue _            = False

---------------
-- Substitution

termSubst :: Id -> Term info -> Term info -> Term info
termSubst x e (TmVar fi y)       = if x == y then e else TmVar fi y
termSubst x e (TmAbs fi y ty t)  = if x == y then TmAbs fi y ty t else
                                     TmAbs fi y ty (termSubst x e t)
termSubst x e (TmApp fi t1 t2)   = TmApp fi (termSubst x e t1)
                                     (termSubst x e t2)
termSubst x e (TmIf fi t1 t2 t3) = TmIf fi (termSubst x e t1)
                                     (termSubst x e t2) (termSubst x e t3)
termSubst x e (TmSucc fi t)      = TmSucc fi (termSubst x e t)
termSubst x e (TmPred fi t)      = TmPred fi (termSubst x e t)
termSubst x e (TmIszero fi t)    = TmIszero fi (termSubst x e t)
termSubst x e (TmFix fi t)       = TmFix fi (termSubst x e t)
termSubst _ _ t                  = t

-------
-- Misc

boolOf :: Term info -> Bool
boolOf (TmTrue _)  = True
boolOf (TmFalse _) = False
boolOf _           = error "boolOfRaw: expected boolean term"

info_of_term t =
  case t of
    TmVar fi _     -> fi
    TmAbs fi _ _ _ -> fi
    TmApp fi _ _   -> fi
    TmTrue fi      -> fi
    TmFalse fi     -> fi
    TmIf fi _ _ _  -> fi
    TmZero fi      -> fi
    TmSucc fi _    -> fi
    TmPred fi _    -> fi
    TmIszero fi _  -> fi
    TmFix fi _     -> fi

info_of_command c =
  case c of
    CBind fi _ _ -> fi
    CEval fi _   -> fi
