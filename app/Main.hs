module Main (main) where

import AST
import Interpreter

main :: IO ()
main = run test1

test1 :: Program
test1 =
  Block
    [ Let "applyTwice" (TFn (TFn TNum TNum) (TFn TNum TNum)) $
        Lambda "f" $
          Lambda "x" $
            App (Var "f") (App (Var "f") (Var "x"))
    , Let "add5" (TFn TNum TNum) $
        Lambda "n" $
          Add (Var "n") (Num 5)
    , Let "apply10" (TFn TNum TNum) $
        App (Var "applyTwice") (Var "add5")
    , Let "counter" TNum (Num 0)
    , While (LessThan (Var "counter") (Num 100)) $
        Block
          [ Mut "counter" (App (Var "apply10") (Var "counter"))
          , Print (Var "counter")
          ]
    ]
