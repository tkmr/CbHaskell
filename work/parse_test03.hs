module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec.Language
--import qualified Text.ParserCombinators.Parsec.Token as P
--import ParsecLanguage( haskellStyle )

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
    where
      op s f assoc = Infix (do { string s; return f }) assoc

factor = do { char '('
            ; x <- expr
            ; char ')'
            ; return x
            }
         <|> number
         <?> "simple expression"

number :: Parser Integer
number = do { ds <- many1 digit
            ; return (read ds)
            }
         <?> "number"

