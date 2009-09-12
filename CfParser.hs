module CfParser where
import Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

--quickcheck
import Test.QuickCheck
import System.Random
import Control.Monad
------------

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
                             reservedNames   = ["return", "total"]
                           , reservedOpNames = ["="]
                           })

reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
lexeme = P.lexeme lexer

---------------------------------

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
semi = lexeme(char ';')

expression = try(assign)
         <|> try(identifier)

assign :: Parser String
assign = lexeme(do{ idn <- identifier
                   ; reservedOp "="
                   ; exp <- expression
                   ; semi
                   ; return idn
                 })
         <?> "assign"



