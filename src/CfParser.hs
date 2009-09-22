module CfParser where
import CfDataType
import Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
    
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (javaStyle {
                             reservedNames   = ["static", "return", "typedef", "struct", "union", "import", "if", "else", "...", "void", "char", "short", "int", "long", "unsigned", "goto", "break", "continue", "for", "while", "label", "sizeof"]
                           , reservedOpNames = ["=", "||", "&&", ">", "<", ">=", "<=", "==", "!=", "|", "^", "&", ">>", "<<", "+", "-", "*", "/", "%", "->"]
                           })

reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
semi = lexeme $ char ';'
       
number :: Parser Int
number = do{ ds <- many1 digit
           ; return (read ds)
           }


--utils--------------------------------------
hasStr :: String -> Parser Bool       
hasStr str = do { result <- option False (do{ reserved str; return True })
                ; return result
                }

eqWithExpression = do { lexeme $ char '='
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

tryOrDefault :: a -> Parser a -> Parser a
tryOrDefault def p = try p
                     <|>
                     do{ return def }

tryOrNothing :: Parser a -> Parser (Maybe a)
tryOrNothing p = do { res <- p
                    ; return $ Just res }
                 <|>
                 return Nothing

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
defvarsParser = do{ isstatic <- lexeme $ hasStr "static"
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
                ; vars <- many defvarsParser
                ; stmts <- many statementParser
                ; lexeme $ char '}'
                ; return $ Block (foldl (++) [] vars) stmts
                }


--statement--------------------------------
statementParser :: Parser Statement
statementParser = tryallParser [ nullStmtParser
                               , labelStmtParser
                               , ifStmtParser
                               , blockStmtParser
                               , expStmtParser
                               , whileStmtParser
                               , forStmtParser
                               , breakStmtParser
                               , contStmtParser
                               , gotoStmtParser
                               , returnStmtParser
                               ]

nullStmtParser = do{ semi
                   ; return NulllineStatement }


labelStmtParser = lexeme $ do{ reserved "label"
                             ; name <- identifier
                             ; char ':'
                             ; return $ LabeledStatement name }

blockStmtParser = do{ b <- blockParser
                    ; return $ BlockStatement b }

ifStmtParser = do{ lexeme $ reserved "if"
                 ; exp <- wrapedChar '(' ')' expressionParser
                 ; thenblock <- statementParser
                 ; elseblock <- tryOrDefault (NulllineStatement) elsePs
                 ; return $ IfStatement exp thenblock elseblock }
    where
      elsePs = do{ lexeme $ reserved "else"
                 ; elseblock <- statementParser
                 ; return elseblock }

expStmtParser = do{ exp <- lexeme $ expressionParser
                  ; semi
                  ; return $ ExpStatement exp
                  }

whileStmtParser = do{ reserved "while"
                    ; exp <- wrapedChar '(' ')' expressionParser
                    ; body <- statementParser
                    ; return $ WhileStatement exp body }
                             
forStmtParser = do{ reserved "for"
                  ; lexeme $ char '('
                  ; exp1 <- expressionParser
                  ; semi
                  ; exp2 <- expressionParser
                  ; semi
                  ; exp3 <- expressionParser
                  ; lexeme $ char ')'
                  ; body <- statementParser
                  ; return $ ForStatement exp1 exp2 exp3 body }

breakStmtParser = do{ reserved "break"
                    ; semi
                    ; return BreakStatement }

contStmtParser = do{ reserved "continue"
                   ; semi
                   ; return ContinueStatement }

gotoStmtParser = do{ reserved "goto"
                   ; name <- identifier
                   ; semi
                   ; return $ GotoStatement name }

returnStmtParser = do{ reserved "return"
                     ; exp <- tryOrDefault (NullExp) expressionParser
                     ; semi
                     ; return $ ReturnStatement exp }

--expression--------------------------------
expressionParser :: Parser Expression
expressionParser = tryallParser [ assignExpParser
                                , exp10Parser ]

exp10Parser :: Parser Expression
exp10Parser = do{ exp1 <- exp9Parser
                ; do{ try $ lexeme $ char '?'
                    ; exp2 <- expressionParser
                    ; lexeme $ char ':'
                    ; exp3 <- exp10Parser
                    ; return $ TreeConditionExp exp1 exp2 exp3
                    }
                  <|>
                  do{ return exp1 }
                }

exp9Parser = twoTermParser exp8Parser ["||"] BoolCondExp
             
exp8Parser = twoTermParser exp7Parser ["&&"] BoolCondExp
             
exp7Parser = twoTermParser exp6Parser [">", "<", "<=", ">=", "==", "!="] BoolCondExp
             
exp6Parser = twoTermParser exp5Parser ["|"] BitcalcExp
             
exp5Parser = twoTermParser exp4Parser ["^"] BitcalcExp
             
exp4Parser = twoTermParser exp3Parser ["&"] BitcalcExp
             
exp3Parser = twoTermParser exp2Parser [">>", "<<"] BitcalcExp
             
exp2Parser = twoTermParser exp1Parser ["+", "-"] MathcalcExp
             
exp1Parser = twoTermParser termExpParser ["*", "/", "%"] MathcalcExp
             
twoTermParser next opes const = do{ expA <- next
                                  ; optparse expA opes
                                  }
    where
      optparse expA []     = do{ return expA }
      optparse expA (o:os) = do{ try $ reservedOp o
                               ; expB <- next
                               ; return $ const expA expB o
                               }
                             <|>
                             optparse expA os

nullExpParser = do{ reserved "null"
                  ; return NullExp }
                   
termExpParser = do{ term <- termParser
                  ; return $ TermExp term }



--term--------------------------------------
termParser :: Parser Term
termParser = do{ try $ lexeme $ char '('
               ; type_ <- typerefParser
               ; lexeme $ char ')'
               ; tm <- termParser
               ; return $ CastTerm type_ tm
               }
             <|>
             unaryTermParser

--prefix             
prefixTerm opes f = do{ op <- tryallParser $ map string opes
                      ; tm <- f
                      ; return $ PrefixCalcTerm op tm
                      }
             
unaryTermParser = tryallParser [ prefixTerm ["++", "--"] unaryTermParser
                               , prefixTerm ["!", "~", "*", "&"] termParser
                               , sizeofTypeTerm
                               , sizeofTerm
                               , postfixTerm ]
    where
      sizeofTypeTerm = do{ reserved "sizeof"
                         ; type_ <- wrapedChar '(' ')' typerefParser
                         ; return $ TypesizeTerm type_ }

      sizeofTerm = do{ reserved "sizeof"
                     ; tm <- unaryTermParser
                     ; return $ SizeTerm tm }


--postfix      
postfixTerm = do{ prim <- primaryTermParser
                ; do{ op <- (try string "++") <|> (try string "--")
                    ; return $ PostfixCalcTerm op prim }
                  <|>
                  do{ exp <- try $ wrapedChar '[' ']' expressionParser
                    ; return $ ArrayrefTerm exp prim }
                  <|>
                  do{ try $ char '.'
                    ; name <- identifier
                    ; return $ StructrefTerm name prim }
                  <|>
                  do{ try $ reservedOp "->"
                    ; name <- identifier
                    ; return $ PointerrefTerm name prim }
                  <|>
                  do{ try $ char '('
                    ; args <- commaSepareted expressionParser
                    ; char ')'
                    ; return $ FunccallTerm args prim }
                  <|>
                  do{ return prim }
                }

primaryTermParser = tryallParser [ numlit, charlit, strlit, varlit, explit ]
    where
      numlit  = do{ num <- number
                  ; return $ NumberLiteral num }

      charlit = do{ chr <- P.charLiteral
                  ; return $ CharLiteral chr }

      strlit  = do{ str <- P.stringLiteral
                  ; return $ StringLiteral str ) 

      varlit  = do{ idt <- identifier
                  ; return $ VarIdentLiteral idt }

      explit  = do{ exp <- wrapedChar '(' ')' expressionParser
                  ; return $ ExpLiteral exp
                  }
                  
-----                    
assignExpParser :: Parser Expression
assignExpParser = do{ target <- termParser
                    ; ope <- operator
                    ; value <- expressionParser
                    ; return $ AssignExp target ope value
                    }
    where
      operator = tryallStr [ "=", "+=", "-=", "*=", "/="
                           , "%=", "&=", "|=", "^=", "<<=", ">>="]

      tryallStr ls = tryallParser $ map (\s -> lexeme $ string s) ls

numberLiteralParser :: Parser Number
numberLiteralParser = lexeme(do{ x <- number
                               ; return x
                               })




