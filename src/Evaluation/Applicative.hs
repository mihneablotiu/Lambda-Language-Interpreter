module Evaluation.Applicative where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State

{-|
    Small-step applicative-order evaluation of a given expression,
    within a given context.
-}
eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval = runState . evalM

evalM :: Expression -> Eval Expression
evalM (Def var e) = modify (M.insert var e) >> return e
evalM (Var var) = gets (M.findWithDefault (Var var) var)
evalM (App (Lambda var1 e1) value@(Lambda var2 e2)) = return $ subst var1 value e1
evalM (App func@(Lambda var e1) e2) = evalM e2 >>= \result -> return $ App func result
evalM (App e1 e2) = evalM e1 >>= \result -> return $ App result e2
evalM expr = return expr
