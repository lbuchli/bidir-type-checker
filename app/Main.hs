module Main (main) where

import AST
import Interpreter

main :: IO ()
main = run test2

test1 :: Program
test1 =
  Block
    [ Let "x" TNum (Num 0)
    , While (LessThan (Var "x") (Num 100)) $
        Block
          [ Mut "x" (Add (Var "x") (Num 1))
          , Print (Var "x")
          ]
    ]

test2 :: Program
test2 = 
  Print (Ternary (Boolean True) (Num 42) (Num 23))
