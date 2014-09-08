-- Source language

module StackCompiler.ArithLang where

-- Atomic arithmetic type
type Unit = Int

-- Expressions
data BinOp   = Plus | Times deriving Show
data UnaryOp = Negate deriving Show
data Exp     = EConst Unit
             | EBinOp BinOp Exp Exp
             | EUnaryOp UnaryOp Exp
             deriving Show

-- Evaluation
evalBinOp b = case b of
    Plus  -> (+)
    Times -> (*)

evalUnaryOp u = case u of
    Negate -> negate

evalExp e = case e of
    EConst n       -> n
    EBinOp b e1 e2 -> (evalBinOp b) (evalExp e1) (evalExp e2)
    EUnaryOp u e   -> (evalUnaryOp u) (evalExp e)
