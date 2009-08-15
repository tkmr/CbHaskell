module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
--import ParsecLanguage( haskellStyle )

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*", "/", "+", "-"] })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
    where
      op s f assoc = Infix (do { reservedOp s; return f } <?> "operator") assoc

factor =  parens expr
      <|> natural
      <?> "simpled expression"

runLex :: Show a => Parser a -> String -> IO ()
runLex p input = run (do { whiteSpace
                         ; x <- p
                         ; eof
                         ; return x
                           }) input



