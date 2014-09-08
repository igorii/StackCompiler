-- Target Language

module StackCompiler.StackLang where

import StackCompiler.ArithLang

-- Expressions
data Instr = IConst Unit
           | IBinOp BinOp
           | IUnaryOp UnaryOp
           deriving Show

-- A program is a list of stack instructions
type Prog  = [Instr]

-- The stack is a basic stack of the atomic type
type Stack = [Unit]

-- Evaluation
evalInstr i s = case i of
    IConst n -> Just (n : s)
    IBinOp b -> case s of
        x : y : zs -> Just $ ((evalBinOp b) x y) : zs
        _ -> Nothing
    IUnaryOp u -> case s of
        x : xs -> Just $ ((evalUnaryOp u) x) : xs
        _ -> Nothing

evalProg p s = case p of
    [] -> Just s
    i : p' -> case evalInstr i s of
        Nothing -> Nothing
        Just s' -> evalProg p' s'
