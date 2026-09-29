{-# LANGUAGE ImportQualifiedPost #-}

module Interpreter (run) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import AST
import Result
import TypeChecker

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
  Ternary c x y -> 
    case eval env c of
      VBool True -> eval env x
      VBool False -> eval env y
      _ -> error "Type invariant broken"

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
