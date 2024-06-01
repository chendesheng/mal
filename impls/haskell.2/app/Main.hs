module Main where

import Step0 (step0_repl)
import System.Environment (getArgs)

main :: IO ()
main = do
    (step : _) <- getArgs
    case step of
        "step0_repl" -> step0_repl
        _ -> error $ "unknown step: " <> step
