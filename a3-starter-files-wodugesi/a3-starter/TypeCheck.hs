module TypeCheck where

import Term
import Type

data TcResult
  = TcOk Type
  | TcError String
  deriving (Show, Eq)

type TcContext = [Type]

lookupCtx :: Int -> TcContext -> Maybe Type
lookupCtx k ctx = if k < 0 || k >= length ctx
                then Nothing
                else Just (ctx !! k)

typeCheck :: TcContext -> Term -> TcResult
typeCheck = error "TODO"
