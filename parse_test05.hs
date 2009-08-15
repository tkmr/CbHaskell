module Main where
import Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (do { whiteSpace
                         ; x <- p
                         ; eof
                         ; return x
                           }) input

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef {
                             reservedNames   = ["return", "total"]
                           , reservedOpNames = ["*", "/", "+", "-"]
                           })

reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
identifier = P.identifier lexer
semi = lexeme( char ';' )

----------------------
receipt :: Parser (Bool, [Int])
receipt = do { ps <- many1 produkt
             ; tt <- total
             ; return ((success ps tt), (ps ++ [tt]))
             }
    where success ls r = (sum ls) == r

produkt :: Parser Int
produkt = do { reserved "return"
             ; i <- price
             ; semi
             ; return (0 - i)
             }
          <|>
          do { identifier
             ; i <- price
             ; semi
             ; return i
          }

total :: Parser Int
total = do { i <- price
           ; reserved "total"
           ; semi
           ; return i
        }

price :: Parser Int
price = lexeme ( do { ds1 <- many1 digit
                    ; char '.'
                    ; ds2 <- count 2 digit
                    ; return (convert 0 (ds1 ++ ds2))
                    })
        <?> "price"
        where
          convert n [] = n
          convert n (d:ds) = convert (10*n + digitToInt d) ds