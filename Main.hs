module Main where

import StackCompiler.Parser
import StackCompiler.StackLang
import StackCompiler.ArithLang
import StackCompiler.Compiler

main :: IO ()
main = putStrLn . show $ evalProg (compile . parseString $ "(1+2*3)") []
