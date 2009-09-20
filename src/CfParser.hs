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

--utils--------------------------------------
hasStr :: String -> Parser Bool       
hasStr str = do { result <- option False (do{ string str; return True })
                ; return result
                }

tryOrNothing :: Parser a -> Parser (Maybe a)
tryOrNothing p = do { res <- p
                    ; return $ Just res }
                 <|>
                 return Nothing

eqWithExpression = do { try(char '=')
                      ; whiteSpace
                      ; exp <- expressionParser
                      ; return exp }

commaSepareted :: Parser a -> Parser [a]
commaSepareted p = do { x <- try(p)
                      ; do { try(lexeme(char ','))
                           ; xs <- commaSepareted p                                
                           ; return (x:xs)
                           }
                        <|> return [x]
                      }
                   <|> return []

wrapedChar :: Char -> Char -> Parser a -> Parser a
wrapedChar s e p = do{ lexeme $ char s
                     ; res <- p
                     ; lexeme $ char e
                     ; return res
                     }
                        
--statements---------------------------------
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

--definition---------------------------------
defvarsParser :: Parser [DefVar]
defvarsParser = do{ isstatic <- StaticProp $ hasStr "static"
                  ; whiteSpace
                  ; typename <- identifier
                  ; res      <- commaSepareted $ defvarParser isstatic typename
                  ; return res
                  }
                <?> "defvarsParser"

defvarParser :: Bool -> String -> Parser DefVar
defvarParser static typename = lexeme(do{ name <- identifier
                                        ; exp <- tryOrNothing eqWithExpression
                                        ; semi
                                        ; return $ DefVar static typename name exp
                                        })

--deffun-----------------------------------                               
deffunParser :: Parser DefFun
deffunParser = do{ isstatic <- StaticProp $ hasStr "static"
                 ; whiteSpace
                 ; typename <- identifier
                 ; name <- identifier
                 ; params <- wrapedChar '(' ')' funcparamParser
                 ; body <- blockParser
                 ; return $ DefFun isstatic typename name params body
                 }

funcparamParser :: Parser FuncParams
funcparamParser = lexeme(do{ try(string "void")
                           ; return VoidParams }
                         <|>
                         do{ params <- commaSepareted paramParser
                           ; do{ try(do{ char ','; string "..." })
                               ; return VariableParams params
                               }
                             <|> return FixedParams params
                           })

paramParser :: Parser Param
paramParser = do{ typename <- identifier
                ; name <- identifier
                ; return $ Param typename name
                }

blockParser :: Parser Block
blockParser = do{ lexeme $ char '{'
                ; vars <- many1 defvarParser
                ; stmts <- many1 statementParser
                ; lexeme $ char '}'
                ; return $ Block vars stmts
                }

--statement--------------------------------

statementParser :: Parser Statement
statementParser = do{ try $ lexeme $ string "if"
                    ; exp <- wrapedChar '(' ')' expressionParser
                    ; thenblock <- blockParser
                    ; do{ try $ lexeme $ string "else"
                        ; elseblock <- blockParser
                        ; return $ IfStatement exp thenblock elseblock
                        }
                      <|> return $ IfStatement exp thenblock (Block [] [])
                    }
                      
--expression--------------------------------
expressionParser :: Parser Expression
expressionParser = try(assignParser)
                   <|> try(do{ term <- termParser
                             ; return $ TermExp term
                             })

termParser :: Parser Term
termParser = do { x <- try $ numberLiteralParser
                ; return $ toTerm x
                }
                   
assignParser :: Parser Expression
assignParser = lexeme(do{ name <- variableParser
                        ; reservedOp "="
                        ; value <- expressionParser
                        ; return $ AssignExp name value
                        })

variableParser :: Parser Name
variableParser = lexeme(do{ name <- identifier
                          ; return name
                          })

numberLiteralParser :: Parser Number
numberLiteralParser = lexeme(do{ x <- number
                               ; return x
                               })




