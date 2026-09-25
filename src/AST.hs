module AST where

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
