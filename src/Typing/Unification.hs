module Typing.Unification where

import Typing.Type
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}
type Unif = StateT Substitution (Except String)

runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:
    
    * an unbound type variable
    
    * a function type.
-}
chainEnd :: Type       -- ^ Type to look up
         -> Unif Type  -- ^ Chain end
chainEnd initialTypeVar@(TypeVar v) = do
    substition <- get
    maybe (return initialTypeVar) 
          (\t -> case t of 
            TypeVar v' -> chainEnd t
            Arrow _ _   -> return t) 
          (M.lookup v substition)
chainEnd t = return t

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck searchTypeVar typeVar@(TypeVar v) = do
    chainEndVariable <- chainEnd typeVar
    case chainEndVariable of
        (TypeVar v') -> return $ searchTypeVar /= v'
        _ -> occCheck searchTypeVar chainEndVariable

occCheck searchTypeVar (Arrow t1 t2) = do
    notFoundInT1 <- occCheck searchTypeVar t1
    if notFoundInT1 then occCheck searchTypeVar t2 else return notFoundInT1

{-|
    Unifies two type expressions.
-}
unify :: Type     -- ^ First type
      -> Type     -- ^ Second type
      -> Unif ()  -- ^ () if the types unify or an exception otherwise
unify initialType@(TypeVar v1) resultingType@(TypeVar v2) = do
    firstChainEnd <- chainEnd initialType
    secondChainEnd <- chainEnd resultingType
    case (firstChainEnd, secondChainEnd) of
        (TypeVar v1', TypeVar v2') -> when (v1' /= v2') $ modify (M.insert v1' secondChainEnd)
        (type1, type2) -> unify type1 type2

unify initialType@(TypeVar v) resultingType@(Arrow t1 t2) = do
    initialTypeChainEnd <- chainEnd initialType
    case initialTypeChainEnd of
        (TypeVar v') -> do
            occCheck <- occCheck v' resultingType
            if not occCheck
            then lift $ throwError $ "Type variable " ++ show initialType ++ " occurs in " ++ show resultingType
            else modify (M.insert v resultingType)
        _ -> unify initialTypeChainEnd resultingType


unify initialType@(Arrow t1 t2) resultingType@(TypeVar v) = unify resultingType initialType
unify initialType@(Arrow t1 t2) resultingType@(Arrow t3 t4) = unify t1 t3 >> unify t2 t4

{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type       -- ^ Target type
           -> Unif Type  -- ^ Resulting type
applySubst typeVar@(TypeVar v) = get >>= \s -> maybe (return typeVar) applySubst (M.lookup v s)
applySubst (Arrow t1 t2) = liftM2 Arrow (applySubst t1) (applySubst t2)
