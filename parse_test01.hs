module Main where

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple = letter

openClose :: Parser Char
openClose = do { char '('
               ; char ')'
               }

parens :: Parser ()
parens = do { char '('
            ; parens
            ; char ')'
            ; parens
            }
         <|> return ()

testOr2 = try(string "(a)")
          <|> string "(b)"

nesting :: Parser Int
nesting = do { char '('
             ; n <- nesting
             ; char ')'
             ; m <- nesting
             ; return (max (n+1) m)
             }
          <|> return 0

word3 :: Parser String
word3 = do { c <- letter
           ; do { cs <- word3
                ; return (c:cs)
                }
             <|> return [c]
           }

word :: Parser String
word = many1 letter <?> "word"

sentence :: Parser [String]
sentence = do { words <- sepBy1 word separator
              ; oneOf ".?!"
              ; return words
              }

separator :: Parser ()
separator = skipMany (space <|> char ',' <?> "")

paragraph :: Parser [[String]]
paragraph = many1 sentence

ace :: Parser Char
ace = do { char 'a' }

acies :: Parser [Char]
acies = do { c <- ace
           ; cs <- acies
           ; return (c:',':cs)
           }
        <|> return []

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x
