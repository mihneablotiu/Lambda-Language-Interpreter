module Evaluation.Substitution where

import Syntax.Expression
import Data.Set

{-|
    Returns the list of free variables in an expression.
-}
freeVarsWithSets :: Expression -> Set String
freeVarsWithSets (Var var) = singleton var
freeVarsWithSets (Lambda var e) = freeVarsWithSets e \\ singleton var
freeVarsWithSets (App e1 e2) = freeVarsWithSets e1 `union` freeVarsWithSets e2

freeVars :: Expression -> [String]
freeVars = toList . freeVarsWithSets
 
{-|
    Performs the substitution of the free occurrences of a variable within
    an expression with another expression.
-}
subst :: String      -- ^ Variable
      -> Expression  -- ^ New expression
      -> Expression  -- ^ Existing expression
      -> Expression  -- ^ Resulting expression
subst toBeReplacedVar newExpr currentExpr@(Var currentVar)
    | toBeReplacedVar == currentVar = newExpr
    | otherwise = currentExpr

subst toBeReplacedVar newExpr currentExpr@(Lambda var e)
    | toBeReplacedVar == var = currentExpr
    | var `notElem` freeVars newExpr = Lambda var (subst toBeReplacedVar newExpr e)
    | otherwise = Lambda varRenamed bodyAfterBetaReduction
                  where
                    varRenamed = var ++ "#" 
                    bodyAfterAlfaConversion = subst var (Var varRenamed) e
                    bodyAfterBetaReduction = subst toBeReplacedVar newExpr bodyAfterAlfaConversion

subst toBeReplacedVar newExpr (App e1 e2) = App (subst toBeReplacedVar newExpr e1) (subst toBeReplacedVar newExpr e2)

