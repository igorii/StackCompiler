-- Compilation from source language to stack machine

module StackCompiler.Compiler where

import StackCompiler.ArithLang
import StackCompiler.StackLang

-- Translation
compile e = case e of
    EConst n       -> (IConst n) : []
    EBinOp b e1 e2 -> compile e2 ++ compile e1 ++ IBinOp b : []
    EUnaryOp u e1  -> compile e1 ++ IUnaryOp u : []
