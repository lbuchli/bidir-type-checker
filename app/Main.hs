module Main (main) where

import MyLib

main :: IO ()
main = run test1

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
