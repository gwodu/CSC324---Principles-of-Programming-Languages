module BigStep where

import Term

type ErrorMessage = String

data EvalResult
  = EvalOk Value
  | EvalError ErrorMessage
  deriving (Eq, Show)

bigStep :: Term -> EvalResult
bigStep _ = error "TODO"
