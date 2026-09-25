{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module MyLib (run, Program, Stmt (..), Expr (..), Type (..)) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Control.Monad (foldM_)

import Result

--------------------------------------------------------------
--                            AST                           --
--------------------------------------------------------------

type Name = String

data Type
  = TNum
  | TBool
  deriving (Eq, Show)

data Expr
  = Add Expr Expr
  | Num Int
  | Boolean Bool
  | LessThan Expr Expr
  | Equal Expr Expr
  | Var Name
  deriving (Show)

data Stmt
  = Let Name Type Expr
  | Mut Name Expr
  | Print Expr
  | While Expr Stmt
  | Block [Stmt]
  deriving (Show)

type Program = Stmt

----------------------------------------------------------------
--                        Type Checker                        --
----------------------------------------------------------------

{-
example tasks:
- Add ternary if
- Add arrays (medium)
- Add functions (hard!)
- Add lambdas (hard!)
- Add user defined types (even harder!)
- Add type polymorphism (you have too much time)
-}

type TyEnv = Map Name Type

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
    return TBool
  (Var v) -> case Map.lookup v env of
    Just t -> return t
    Nothing -> Err $ "Variable not in scope: " ++ v

check :: TyEnv -> Expr -> Type -> Result String ()
check env = \cases
  -- there might be extra cases here, e.g. when implementing lambda
  x t -> do
    t' <- infer env x
    if t == t'
      then return ()
      else
        Err $
          "Couldn't match expected type "
            ++ show t
            ++ " with inferred type "
            ++ show t'
            ++ " in expression "
            ++ show x

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

----------------------------------------------------------------
--                        Interpreter                         --
----------------------------------------------------------------

-- Runtime values stored in the environment
data Value
  = VNum Int
  | VBool Bool
  deriving (Eq, Show)

type Env = Map Name Value

eval :: Env -> Expr -> Value
eval env expr = case expr of
  Num n -> VNum n
  Boolean b -> VBool b
  Var name -> env Map.! name
  Add e1 e2 ->
    let VNum v1 = eval env e1
        VNum v2 = eval env e2
     in VNum (v1 + v2)
  LessThan e1 e2 ->
    let VNum v1 = eval env e1
        VNum v2 = eval env e2
     in VBool (v1 < v2)
  Equal e1 e2 ->
    let v1 = eval env e1
        v2 = eval env e2
     in VBool (v1 == v2)

execStmt :: Env -> Stmt -> IO Env
execStmt env stmt = case stmt of
  Let name _ expr -> do
    let val = eval env expr
    pure $ Map.insert name val env
  Mut name expr -> do
    let val = eval env expr
    pure $ Map.insert name val env
  Print expr -> do
    let val = eval env expr
    case val of
      VNum n -> print n
      VBool b -> print b
    pure env
  While cond body -> do
    case eval env cond of
      VBool True -> do
        env' <- execStmt env body
        execStmt env' (While cond body)
      VBool False ->
        pure env
      _ -> error "Type invariant broken"
  Block stmts ->
    foldl (\ioEnv s -> ioEnv >>= \e -> execStmt e s) (pure env) stmts

run :: Program -> IO ()
run prog = case checkProgram prog of
  Ok () -> do
    _ <- execStmt Map.empty prog
    pure ()
  Err err -> putStrLn $ "Typechecking failed: " ++ err
