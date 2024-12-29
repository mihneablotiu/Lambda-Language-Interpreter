module Syntax.Expression where

import qualified Data.Map as M
import Control.Monad.State

data Expression
    = Var String
    | Lambda String Expression
    | App Expression Expression
    | Def String Expression
    deriving (Eq)

instance Show Expression where
    show (Var var) = var
    show (Lambda var e) = "\\" ++ var ++ "." ++ show e
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Def var e) = var ++ " = " ++ show e

type Context = M.Map String Expression

type Eval a = State Context a
