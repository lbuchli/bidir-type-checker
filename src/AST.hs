module AST where

type Name = String

data Type
  = TNum
  | TBool
  | TFn Type Type
  deriving (Eq, Show)

data Expr
  = Add Expr Expr
  | Num Int
  | Boolean Bool
  | LessThan Expr Expr
  | Equal Expr Expr
  | Var Name
  | Lambda Name Expr
  | App Expr Expr
  | Ann Expr Type
  deriving (Show)

data Stmt
  = Let Name Type Expr
  | Mut Name Expr
  | Print Expr
  | While Expr Stmt
  | Block [Stmt]
  deriving (Show)

type Program = Stmt
