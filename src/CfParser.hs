module CfParser where
import CfDataType
import Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
    
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
                             reservedNames   = ["static", "return", "typedef", "struct", "union", "import", "if", "else", "...", "void", "char", "short", "int", "long", "unsigned"]
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
hasStr str = do { result <- option False (do{ reserved str; return True })
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

separetedByChar :: Char -> Parser a -> Parser [a]
separetedByChar c p = do { x <- try(p)
                         ; do { try(lexeme(char c))
                              ; xs <- separetedByChar c p
                              ; return (x:xs)
                              }
                           <|> return [x]
                         }
                      <|> return []

commaSepareted :: Parser a -> Parser [a]
commaSepareted = separetedByChar ','
    
wrapedChar :: Char -> Char -> Parser a -> Parser a
wrapedChar s e p = do{ lexeme $ char s
                     ; res <- p
                     ; lexeme $ char e
                     ; return res
                     }

tryallParser :: [Parser a] -> Parser a
tryallParser (p:[]) = p
tryallParser (p:ps) = do{ res <- try p; return res }
                      <|>
                      tryallParser ps

--statements---------------------------------
impStatementsParser :: Parser ImportStatements
impStatementsParser = do { imps <- many1 impStatementParser
                         ; return $ ImportStatements imps
                         }
                      
impStatementParser :: Parser ImportStatement
impStatementParser = lexeme(do{ reserved "import"
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
defvarsParser = do{ isstatic <- hasStr "static"
                  ; whiteSpace
                  ; type_ <- typeRefParser
                  ; res   <- commaSepareted $ defvarParser isstatic type_
                  ; return res
                  }
                <?> "defvarsParser"

defvarParser :: Bool -> Type -> Parser DefVar
defvarParser static type_ = lexeme(do{ name <- identifier
                                        ; exp <- tryOrNothing eqWithExpression
                                        ; semi
                                        ; return $ DefVar (StaticProp static) type_ name exp
                                        })

deffunParser :: Parser DefFun
deffunParser = do{ isstatic <- lexeme $ hasStr "static"
                 ; typeref <- typeRefParser
                 ; name <- identifier
                 ; params <- wrapedChar '(' ')' funcparamParser
                 ; body <- blockParser
                 ; return $ DefFun (StaticProp isstatic) typeref name params body
                 }

funcparamParser :: Parser FuncParams
funcparamParser = lexeme(do{ try(reserved "void")
                           ; return VoidParams
                           }
                        <|> subparser [])
    where
      subparser params = do{ param <- try paramParser
                           ; do { try $ lexeme $ char ','
                                ; do{ try $ lexeme $ reserved "..."
                                    ; return $ VariableParams $ reverse (param:params)
                                    }
                                  <|> subparser (param:params)
                                }
                             <|> do{ return $ FixedParams $ reverse (param:params) }
                           }

defstructParser :: Parser DefStruct
defstructParser = do{ lexeme $ reserved "struct"
                    ; name <- identifier
                    ; params <- wrapedChar '{' '}' $ separetedByChar ';' paramParser
                    ; return $ DefStruct name params
                    }

defunionParser :: Parser DefUnion
defunionParser = do{ lexeme $ reserved "union"
                    ; name <- identifier
                    ; params <- wrapedChar '{' '}' $ separetedByChar ';' paramParser
                    ; return $ DefUnion name params
                    }

deftypeParser :: Parser DefType
deftypeParser = do{ lexeme $ reserved "typedef"
                  ; typeref <- typeRefParser
                  ; name <- identifier
                  ; semi
                  ; return $ DefType typeref name
                  }

typeRefParser :: Parser TypeRef
typeRefParser = lexeme $ do{ base <- typerefBaseParser
                           ; options <- typerefOptsParser
                           ; return $ TypeRef base options
                           }

typerefBaseParser :: Parser TyperefBase
typerefBaseParser = lexeme $
      tryallParser [ simple VoidType
                   , simple CharType
                   , simple IntType
                   , simple ShortType
                   , simple LongType
                   , simple UnsignedCharType
                   , simple UnsignedShortType
                   , simple UnsignedIntType
                   , simple UnsignedLongType
                   , simpleWithName "struct" StructType
                   , simpleWithName "union" UnionType
                   , originalType ]
    where                             
      simple f = do{ lexeme $ reserved $ show (f)
                   ; return (f) }

      simpleWithName str f = do{ lexeme $ reserved str
                               ; name <- identifier
                               ; return $ f name }

      originalType = do{ name <- identifier
                       ; if (isTypeDefined name)
                         then return $ OriginalType name
                         else do{ string "this original type didn't define, yet. how can I define ParseError logic?"; return VoidType }
                       }

typerefOptsParser :: Parser [TyperefOption]
typerefOptsParser = many $ tryallParser [ varray, array, pointer, funcp ]
    where
      varray = do{ lexeme $ char '['
                 ; lexeme $ char ']'
                 ; return NonLimitArrayOption }

      array = do{ lexeme $ char '['
                ; num <- number
                ; lexeme $ char ']'
                ; return $ LimitedArrayOption num }

      pointer = do{ lexeme $ char '*'
                  ; return PointerOption }

      funcp = do{ lexeme $ char '('
                ; types <- many typeRefParser
                ; lexeme $ char ')'
                ; return $ FuncPointerOption types }         
                     
--utils---
isTypeDefined :: String -> Bool
isTypeDefined name = False

paramParser :: Parser Param
paramParser = do{ type_ <- typeRefParser
                ; name <- identifier
                ; return $ Param type_ name
                }
              <?> "paramParser"

blockParser :: Parser Block
blockParser = do{ lexeme $ char '{'
                ; vars <- many1 defvarsParser
                ; stmts <- many1 statementParser
                ; lexeme $ char '}'
                ; return $ Block (foldl (++) [] vars) stmts
                }


--statement--------------------------------
statementParser :: Parser Statement
statementParser = do{ try $ lexeme $ reserved "if"
                    ; exp <- wrapedChar '(' ')' expressionParser
                    ; thenblock <- blockParser
                    ; do{ try $ lexeme $ reserved "else"
                        ; elseblock <- blockParser
                        ; return $ IfStatement exp thenblock elseblock
                        }
                      <|>
                      do{ return $ IfStatement exp thenblock (Block [] []) }
                    }
                  <|>
                  do{ exp <- try $ lexeme $ expressionParser
                    ; semi
                    ; return $ ExpStatement exp
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




