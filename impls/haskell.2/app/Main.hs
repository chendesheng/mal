module Main where

import Step0 (step0_repl)
import Step1 (step1_read_print)
import Step2 (step2_eval)
import System.Environment (getArgs)

main :: IO ()
main = do
    (step : _) <- getArgs
    case step of
        "step0_repl" -> step0_repl
        "step1_read_print" -> step1_read_print
        "step2_eval" -> step2_eval
        _ -> error $ "unknown step: " <> step
