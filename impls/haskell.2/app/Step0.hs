{-# LANGUAGE OverloadedStrings #-}

module Step0 where

import Data.Text (Text)
import Data.Text.IO (getLine, putStrLn)
import Prelude hiding (getLine, print, putStrLn, read)

read :: IO Text
read = do
    print  "user> "
    getLine

eval :: Text -> Text
eval s = s

print :: Text -> IO ()
print = putStrLn

step0_repl :: IO ()
step0_repl = do
    input <- read
    print $ eval input
    step0_repl
