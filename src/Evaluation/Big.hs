module Evaluation.Big where

import Syntax.Expression
import Control.Monad.State

{-|
    Big-step evaluation of a given expression, within a given context.
    The evaluation should stop when either the value is reached,
    or the expression cannot be reduced further.
    
    The first argument is the small-step evaluation function.
-}
evalBig :: (Expression -> Context -> (Expression, Context))  -- ^ Small-stepper
        -> Expression             -- ^ Expression to be evaluated
        -> Context                -- ^ Context where the evaluation takes place
        -> (Expression, Context)  -- ^ Evaluation result,
                                  --   together with a possibly enriched context
                                  --   in case of definition
evalBig oneStepper expr = runState $ evalBigM (state . oneStepper) expr

evalBigM :: (Expression -> Eval Expression) -> Expression -> Eval Expression
evalBigM oneStepper expr = oneStepper expr >>= \result -> if result == expr then return expr else evalBigM oneStepper result

{-|
    Big-step evaluation of a list of expressions, starting with
    the given context and using it throughout the entire list,
    for propagating the encountered definitions.
    
    The first argument is the small-step evaluation function.
-}
evalList :: (Expression -> Context -> (Expression, Context))
         -> [Expression]
         -> Context
         -> ([Expression], Context)
evalList oneStepper exprs = runState $ evalListM (state . oneStepper) exprs

evalListM :: (Expression -> Eval Expression) -> [Expression] -> Eval [Expression]
evalListM = mapM . evalBigM
