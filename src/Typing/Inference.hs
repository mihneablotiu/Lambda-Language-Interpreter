module Typing.Inference where

import Syntax.Expression
import Typing.Type
import Typing.Unification
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

{-|
    The type of inference state.
    
    Should comprise:
    
    * The global typing context
    
    * The type variable counter.
-}
data TypingState = TypingState
    { context :: TypingContext
    , counter :: Counter
    } deriving Show
    
{-|
    The type of the inference mechanism.
    
    Should expose the following:
    
    * Access to the inference state (State)
    
    * Acces to the local typing context (Reader)

    * A means for storing unification constraints (Writer) -- Why are we storing unification constraints and we are not applying them at each step?
-}
type Infer = ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState)) -- Why we don't receive the substitution? We should apply it at each step while unifying.

runInfer :: Infer a        -- ^ Expression to type
         -> TypingContext  -- ^ Local context
         -> TypingContext  -- ^ Global context
         -> Counter        -- ^ Current type variable counter
         -> (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt = evalState (runWriterT $ runReaderT inf loc) $ 
                                      TypingState glob cnt

{-|
    Generates a copy of the given type.
    
    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy initialType = fst $ runInfer (copyM initialType) M.empty M.empty 0

{-|
    The type inference function, wich synthesizes the type of the given
    expression.
    
    Should rely on 'inferM' below.
-}
infer :: Expression          -- ^ Expression to type
      -> TypingContext       -- ^ Local context
      -> TypingContext       -- ^ Global context
      -> Substitution        -- ^ Substitution
      -> Counter             -- ^ Current type variable counter
      -> Either String Type  -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error 
                             --   message.
infer expr loc glob subst cnt = let
    (typeExpr, unifications) = runInfer (inferM expr) loc glob cnt -- we try to infer the type of the expression
    resultAfterUnification = traverse (\(initType, resultType) -> unify initType resultType) unifications -- we check if the unification constraints can be satisfied
    in case runUnif resultAfterUnification subst of
        Right (_, subst') -> fst <$> runUnif (applySubst typeExpr) subst' -- if the unification constraints can be satisfied, we apply the substitution to the type of the expression
        Left errorMessage -> Left errorMessage -- if the unification constraints cannot be satisfied, we return an error message

{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = do
    globalTypingState <- get -- get the global typing state because here we have the counter
    let currentCounter = counter globalTypingState -- get the current counter
    let newGeneratedTypeVar = TypeVar $ "t" ++ show currentCounter -- create a new type variable
    put $ globalTypingState { counter = currentCounter + 1 } -- update the global typing state with the new counter
    return newGeneratedTypeVar

{-|
    See 'copy'.
-}
type CopyContext = M.Map String Type

copyM :: Type -> Infer Type
copyM initialType = do
    (result, _) <- copyM' initialType M.empty
    return result

copyM' :: Type -> CopyContext -> Infer (Type, CopyContext) -- Why I cannot use a separate State monad? I have to purposely send and return the context back or use another monadic StateT?
copyM' (TypeVar v) copyContext = do
    case M.lookup v copyContext of
        Just t -> return (t, copyContext)
        Nothing -> do
            tv <- newTypeVar
            return (tv, M.insert v tv copyContext)
copyM' (Arrow t1 t2) copyContext = do
    (t1', copyContext') <- copyM' t1 copyContext
    (t2', copyContext'') <- copyM' t2 copyContext'
    return (Arrow t1' t2', copyContext'')

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type
inferM (Var v) = do
    localTypingContext <- ask
    case M.lookup v localTypingContext of -- we search for the type of the variable in the local typing context
        Just t -> return t -- if we find it, we return it
        Nothing -> do
            globalTypingState <- get
            case M.lookup v (context globalTypingState) of -- if we don't find it, we search in the global typing context
                Just t -> copyM t -- if we find it, we return a copy of it because we are in a strong polymorphism situation (why can I here call copyM t but not (return (copy t))?
                Nothing -> do
                    tv <- newTypeVar -- if we don't find it in the global typing context, we generate a new type variable
                    return tv -- how can this happen? Such that a variable is not present in the global typing context or in the local typing context?
                              -- And also if we generate a new type variable we should update the local/global typing state with the new type variable, right?

inferM (Lambda v e) = do
    tv <- newTypeVar -- we generate a new type variable for the new lambda variable
    expressionType <- local (M.insert v tv) (inferM e) -- we infer the type of the expression e in the local typing context with the new lambda variable
    return (Arrow tv expressionType) -- we return the type of the lambda expression

inferM (App e1 e2) = do
    firstExpressionType <- inferM e1 -- we infer the type of the first expression

    leftFunctionType <- newTypeVar
    rightFunctionType <- newTypeVar

    let functionType = Arrow leftFunctionType rightFunctionType -- the type of the first expression should unify with a function type

    tell [(firstExpressionType, functionType)] -- this is the unification constraint

    secondExpressionType <- inferM e2 -- we infer the type of the second expression

    tell [(secondExpressionType, leftFunctionType)] -- this is the unification constraint such that the second expression type should unify with the left function type

    return rightFunctionType -- we return the right function type which is the type of the application expression

inferM (Def v e) = do
    tv <- newTypeVar -- we generate a new type variable for the new definition variable
    expressionType <- local (M.insert v tv) (inferM e) -- we infer the type of the expression e in the local typing context with the new definition variable

    tell [(expressionType, tv)] -- this is the unification constraint such that the expression type should unify with the type variable

    modify $ \globalTypingState -> globalTypingState { context = M.insert v tv (context globalTypingState) } -- we update the global typing state with the new definition variable

    return expressionType -- we return the expression type which is the type of the definition expression
