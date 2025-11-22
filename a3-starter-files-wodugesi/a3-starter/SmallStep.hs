module SmallStep where

import Term
import DeBruijn

type ErrorMessage = String

data StepResult
  = StepOk Term
  | StepError ErrorMessage
  deriving (Eq, Show)

isVal :: Term -> Bool
isVal t = isBoolVal t
          || isNumVal t
          || isLambdaForm t
          || isRecordVal t
          || isPairVal t
          || isSumVal t

isBoolVal :: Term -> Bool
isBoolVal TmTrue = True
isBoolVal TmFalse = True
isBoolVal _ = False

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc t) = isNumVal t
isNumVal _ = False

isLambdaForm :: Term -> Bool
isLambdaForm (TmAnnAbs _ _) = True
isLambdaForm _ = False

isRecordVal :: Term -> Bool
isRecordVal (TmRecord pairs) = all (isVal . snd) pairs
isRecordVal _ = False

isPairVal :: Term -> Bool
isPairVal (TmPair t1 t2) = isVal t1 && isVal t2
isPairVal _ = False

isSumVal :: Term -> Bool
isSumVal (TmInl t _) = isVal t
isSumVal (TmInr t _) = isVal t
isSumVal _ = False


oneStep :: Term -> StepResult
oneStep (TmIf TmTrue t1 _) = StepOk t1                   -- E-IfTrue
oneStep (TmIf TmFalse _ t2) = StepOk t2                  -- E-IfFalse
oneStep (TmIf t t1 t2) =                                 -- E-If
  case oneStep t of
    StepOk t' -> StepOk (TmIf t' t1 t2)
    StepError msg -> StepError ("The condition cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmSucc t) =                                     -- E-Succ
  case oneStep t of
    StepOk t' -> StepOk (TmSucc t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmPred TmZero) = StepOk TmZero                  -- E-PredZero
oneStep (TmPred (TmSucc t))                              -- E-PredSucc
  | isNumVal t = StepOk t
oneStep (TmPred t) =                                     -- E-Pred
  case oneStep t of
    StepOk t' -> StepOk (TmPred t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmIsZero TmZero) = StepOk TmTrue                -- E-IsZeroZero
oneStep (TmIsZero (TmSucc t))                            -- E-IsZeroSucc
  | isNumVal t = StepOk TmFalse
oneStep (TmIsZero t) =                                   -- E-IsZero
  case oneStep t of
    StepOk t' -> StepOk (TmIsZero t')
    StepError msg -> StepError ("The argument cannot be stepped: "
                                ++ show t ++ "\n"
                                ++ msg)
oneStep (TmApp (TmAnnAbs ty t1) t2)                      -- E-AppAbs
  | isVal t2 = StepOk (shifting (-1) 0
                       (subst 0 (shifting 1 0 t2) t1))
oneStep (TmApp t1 t2)
  | isVal t1 =                                           -- E-App2
    case oneStep t2 of
      StepOk t2' -> StepOk (TmApp t1 t2')
      StepError msg -> StepError ("The RHS cannot be stepped: "
                                  ++ show t2 ++ "\n"
                                  ++ msg)
  | otherwise =
    case oneStep t1 of
      StepOk t1' -> StepOk (TmApp t1' t2)                -- E-App1
      StepError msg -> StepError ("The LHS cannot be stepped: "
                                  ++ show t1 ++ "\n"
                                  ++ msg)

-- Records: E-ProjRcd, E-Proj, E-Rcd
oneStep (TmProj (TmRecord fields) label) = case lookup label fields of
  Just value -> StepOk value
  Nothing -> StepError ("Label " ++ label ++ " not found")
oneStep (TmProj t label) = case oneStep t of
  StepOk t' -> StepOk (TmProj t' label)
  err -> err
oneStep (TmRecord fields) = 
  let stepOneField ((l, v):rest) = if isVal v
                                    then case stepOneField rest of
                                           Just (rest', changed) -> Just ((l, v):rest', changed)
                                           Nothing -> Nothing
                                    else case oneStep v of
                                           StepOk v' -> Just ((l, v'):rest, True)
                                           err -> Nothing
      stepOneField [] = Nothing
  in case stepOneField fields of
       Just (fields', True) -> StepOk (TmRecord fields')
       _ -> StepError ("Cannot step record: " ++ show (TmRecord fields))

-- Pairs: E-PairBeta1, E-PairBeta2, E-Proj1, E-Proj2, E-Pair1, E-Pair2
oneStep (TmFst (TmPair v1 v2)) | isVal v1 && isVal v2 = StepOk v1
oneStep (TmFst t) = case oneStep t of
  StepOk t' -> StepOk (TmFst t')
  err -> err
oneStep (TmSnd (TmPair v1 v2)) | isVal v1 && isVal v2 = StepOk v2
oneStep (TmSnd t) = case oneStep t of
  StepOk t' -> StepOk (TmSnd t')
  err -> err
oneStep (TmPair v1 t2) | isVal v1 = case oneStep t2 of
  StepOk t2' -> StepOk (TmPair v1 t2')
  err -> err
oneStep (TmPair t1 t2) = case oneStep t1 of
  StepOk t1' -> StepOk (TmPair t1' t2)
  err -> err

-- Sums: E-CaseInl, E-CaseInr, E-Case, E-Inl, E-Inr
oneStep (TmMatch (TmInl v _) t1 t2) | isVal v = StepOk (shifting (-1) 0 (subst 0 (shifting 1 0 v) t1))
oneStep (TmMatch (TmInr v _) t1 t2) | isVal v = StepOk (shifting (-1) 0 (subst 0 (shifting 1 0 v) t2))
oneStep (TmMatch t t1 t2) = case oneStep t of
  StepOk t' -> StepOk (TmMatch t' t1 t2)
  err -> err
oneStep (TmInl t ty) = case oneStep t of
  StepOk t' -> StepOk (TmInl t' ty)
  err -> err
oneStep (TmInr t ty) = case oneStep t of
  StepOk t' -> StepOk (TmInr t' ty)
  err -> err

oneStep t = StepError ("No applicable evaluation rule for: " ++ show t)

multiStep :: Term -> Term
multiStep t = if isVal t
              then t
              else case oneStep t of
                     StepOk t' -> multiStep t'
                     StepError _ -> t
