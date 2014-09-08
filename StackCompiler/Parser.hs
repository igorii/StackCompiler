-- Arithmetic Language Parser

module StackCompiler.Parser 
    ( parseString
    , parseFile
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import StackCompiler.ArithLang

-- Entry

parseString str = 
    case parse whileParser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile file = do
    program <- readFile file
    case parse whileParser "" program of
        Left e -> print e >> fail "parse error"
        Right r -> return r

-- Private

languageDef =
    emptyDef { Token.commentLine = "//"
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.reservedNames = []
             , Token.reservedOpNames = ["+", "*"]
             }

lexer      = Token.makeTokenParser languageDef
identifier = Token.identifier      lexer
reserved   = Token.reserved        lexer
reservedOp = Token.reservedOp      lexer
parens     = Token.parens          lexer
integer    = Token.integer         lexer
whitespace = Token.whiteSpace      lexer

arithExpression = buildExpressionParser arithOperators arithTerm
arithOperators = [ [Prefix (reservedOp "-" >> return (EUnaryOp Negate ))]
                 , [Infix  (reservedOp "*" >> return (EBinOp   Times  )) AssocLeft]
                 , [Infix  (reservedOp "+" >> return (EBinOp   Plus   )) AssocLeft]
                 ]

parseInt = do
    nn <- many1 digit
    return (read nn :: Int)

whileParser = whitespace >> arithTerm
arithTerm = parenATerm <|> atomTerm
parenATerm = do
    whitespace 
    r <- parens arithExpression
    whitespace
    return r

atomTerm = do
    whitespace
    r <- parseInt
    whitespace
    return $ EConst r
