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

 type Token = (SourcePos, Tok)
 data Tok   = Identifier String
            | Reserved   String
            | Symbol     String
            | Price      Int
              deriving Show

scanner :: [Char] -> ([Token], [String])

type MyParser a = GenParser Char () a

