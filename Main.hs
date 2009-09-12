module Main where
import CfParser
import Text.ParserCombinators.Parsec
import Test.QuickCheck

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

prop_assign xs = isString $ runLex assign "hoge = age"
    where
      types = xs::[Char]

isString io = True