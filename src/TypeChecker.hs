{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Control.Monad (foldM_)

import AST
import Result

type TyEnv = Map Name Type

-- here are rules with infer (=>) in their conclusion
infer :: TyEnv -> Expr -> Result String Type
infer env = \case
  (Add x y) -> do
    check env x TNum
    check env y TNum
    return TNum
  (Num _) -> return TNum
  (Boolean _) -> return TBool
  (LessThan x y) -> do
    check env x TNum
    check env y TNum
    return TBool
  (Equal x y) -> do
    tx <- infer env x
    check env y tx
    case tx of
      TBool -> return TBool
      TNum -> return TBool
      _ -> Err $ "Cannot compare values of type " ++ show tx
  (Var v) -> case Map.lookup v env of
    Just t -> return t
    Nothing -> Err $ "Variable not in scope: " ++ v
  (App f x) -> do
    tf <- infer env f
    case tf of
      (TFn a b) -> do
        check env x a
        return b
      _ -> Err $ "Cannot apply " ++ show x ++ " to non-function type " ++ show tf
  (Ann x t) -> do
    check env x t
    return t
  x -> Err $ "Cannot infer type of " ++ show x ++ ". Add explicit type annotations."

-- here are rules with check (<=) in their conclusion
check :: TyEnv -> Expr -> Type -> Result String ()
check env = \cases
  (Lambda v x) (TFn ta tb) -> do
    check (Map.insert v ta env) x tb
  x t -> do
    t' <- infer env x
    if t == t'
      then return () -- mode switch
      else
        Err $
          "Couldn't match expected type "
            ++ show t
            ++ " with inferred type "
            ++ show t'
            ++ " in expression "
            ++ show x

-- you can safely ignore this unless you are adding or modifying a statement
checkProgram :: Program -> Result String ()
checkProgram p = do
  _ <- check_stmt Map.empty p
  return ()
  where
    check_stmt :: TyEnv -> Stmt -> Result String TyEnv
    check_stmt env = \case
      (Let v t rhs) -> do
        check env rhs t
        return $ Map.insert v t env
      (Mut v rhs) -> case Map.lookup v env of
        Just t -> do
          check env rhs t
          return env
        Nothing -> Err $ "Undefined variable: " ++ v
      (Print x) -> do
        infer env x
        return env
      (While cond stmt) -> do
        check env cond TBool
        _ <- check_stmt env stmt
        return env
      (Block stmts) -> do
        foldM_ check_stmt env stmts
        return env
