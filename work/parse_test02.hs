module Main where
import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                Left err -> do { putStr "parse error at "
                               ; print err
                               }
                Right x  -> print x


host :: Parser String
host = many1 letter

path :: Parser String
path = many1 letter

protocol :: Parser String
protocol = string "http"
           <|> string "ftp"
           <|> string "https"

url :: Parser [String]
url = do { prt <- protocol
         ; string "://"
         ; hot <- host
         ; char '/'
         ; pat <- path <|> string ""
         ; return (prt:hot:pat:[])
         }
