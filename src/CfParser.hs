module CfParser where
import CfDataType
import Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
    
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
                             reservedNames   = ["return"]
                           , reservedOpNames = ["="]
                           })

reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
semi = lexeme(char ';')

number :: Parser Int
number = do{ ds <- many1 digit
           ; return (read ds)
           }
         <?> "number"
       
-----------------------------------
impStatementsParser :: Parser ImportStatements
impStatementsParser = do { imps <- many1 impStatementParser
                         ; return $ ImportStatements imps
                         }
                      
impStatementParser :: Parser ImportStatement
impStatementParser = lexeme(do{ string "import"
                              ; whiteSpace
                              ; names <- namespace
                              ; semi
                              ; return $ ImportStatement names
                              })
                     
namespace :: Parser [String]
namespace = lexeme(do{ name <- identifier
                     ; do { try(char '.')
                          ; names <- namespace
                          ; return (name:names)
                          }
                       <|> return [name]
                     })

expressionParser :: Parser Expression
expressionParser = try(assignParser)
               <|> try(do{ term <- termParser
                         ; return $ TermExp term
                       })

termParser :: Parser Term
termParser = do { x <- try(numberLiteralParser)
                ; return $ toTerm x
                }
                   
assignParser :: Parser Expression
assignParser = lexeme(do{ name <- variableParser
                        ; reservedOp "="
                        ; value <- expressionParser
                        ; return $ AssignExp name value
                        })

variableParser :: Parser VariableName
variableParser = lexeme(do{ name <- identifier
                          ; return name
                          })

numberLiteralParser :: Parser Number
numberLiteralParser = lexeme(do{ x <- number
                               ; return x
                               })




