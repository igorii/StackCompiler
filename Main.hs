module Main where

import StackCompiler.StackLang
import StackCompiler.ArithLang
import StackCompiler.Compiler

main :: IO ()
main = putStrLn . show $ evalProg (compile
    (EBinOp Plus
            (EBinOp Plus
                    (EConst 1)
                    (EConst 8))
            (EBinOp Times
                    (EConst 8)
                    (EUnaryOp Negate (EConst 9)))))
    []
