module BigStep where

import Term

type ErrorMessage = String

data EvalResult
  = EvalOk Value
  | EvalError ErrorMessage
  deriving (Eq, Show)

bigStep :: Env -> NamedTerm -> EvalResult
bigStep env t = error "TODO"
