module SmallStep where

import Term

type ErrorMessage = String

data StepResult
  = StepOk Term
  | StepError ErrorMessage
  deriving (Eq, Show)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

oneStep :: Term -> StepResult
oneStep _ = error "TODO"

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t
