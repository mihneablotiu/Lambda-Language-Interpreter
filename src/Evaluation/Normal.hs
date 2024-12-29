module Evaluation.Normal where

import Syntax.Expression
import Evaluation.Substitution
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Syntax.Expression (Context, Expression)

-- newtype ExceptT e m a = ExceptT (m (Either e a))
-- type Eval a = ExceptT String (State Context) a
--             = ExceptT (State Context (Either String a))

-- runExceptT :: ExceptT e m a -> m (Either e a)
-- runExceptT => State Context (Either String a)

-- runState :: State s a -> s -> (a, s)
-- runState => Context -> (Either String a, Context) (pica pasul curent)

-- MyEval Expression = ExceptT String (State Context) Expression
--                   = ExceptT String (State Context) Expression

-- runExceptT :: ExceptT e m a -> m (Either e a)
-- runExceptT => State Context (Either String Expression)

-- runState :: State s a -> s -> (a, s)
-- runState => Context -> (Either String Expression, Context)


-- newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
-- type Eval a = StateT Context (Except String) a
--             = StateT Context -> (Except String) String (a, Context)

-- runStateT :: StateT s m a -> s -> m (a, s)
-- runStateT => Context -> Either String (a, Context) (pica tot procesul)

{-|
    Small-step normal-order evaluation of a given expression,
    within a given context.
-}

{-

type MyEval a = StateT Context (ExceptT String IO) a
              = StateT Context -> (ExceptT String IO) (a, Context)

runStateT => Context -> (ExceptT String IO) (a, Context)
primim context => (ExceptT String IO) (a, Context)

runExceptT => IO (Either String (a, Context))

-}

-- testValue :: [Expression]
-- testValue = [Def "true" (Lambda "x" (Lambda "y" (Var "x"))),
--              Def "false" (Lambda "x" (Lambda "y" (Var "y"))),
--              Def "not" (Lambda "x" (App (App (Var "x") (Var "false")) (Var "true"))),
--              App (Var "not") (Var "true"),
--              App (Var "not") (Var "false"),
--              Var "not"]

-- type MyEval a = StateT Context (ExceptT String IO) a

-- eval :: Expression -> Context -> IO (Either String (Expression, Context))
-- eval expr initialCtx = runExceptT $ runStateT (evalM expr) initialCtx

-- evalM :: Expression -> MyEval Expression
-- evalM (Def var e) = modify (M.insert var e) >> return e
-- evalM (Var var) = do
--     ctx <- get
--     case M.lookup var ctx of
--         Just e -> return e
--         Nothing -> do 
--             liftIO $ putStrLn "Variable not found"
--             throwError $ "Variable " ++ var ++ " not found"

-- evalM (App (Lambda var e1) e2) = return $ subst var e2 e1
-- evalM (App e1 e2) = evalM e1 >>= \result -> return $ App result e2
-- evalM e = return e

eval :: Expression             -- ^ Expression to be evaluated
     -> Context                -- ^ Context where the evaluation takes place
     -> (Expression, Context)  -- ^ Evaluation result, together with a possibly
                               --   enriched context, in case of definition
eval = runState . evalM

evalM :: Expression -> Eval Expression
evalM (Def var e) = modify (M.insert var e) >> return e
evalM (Var var) = gets (M.findWithDefault (Var var) var)
evalM (App (Lambda var e1) e2) = return $ subst var e2 e1
evalM (App e1 e2) = evalM e1 >>= \result -> return $ App result e2
evalM e = return e
