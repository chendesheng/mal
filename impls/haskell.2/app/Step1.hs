{-# LANGUAGE OverloadedStrings #-}

module Step1 where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStrLn)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, many, manyTill, parse)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (getLine, print, putStrLn, read)

type Parser = Parsec Void Text

data MalAst
    = MalList [MalAst]
    | MalVector [MalAst]
    | MalMap [(MalAst, MalAst)]
    | MalSymbol Text
    | MalKeyword Text
    | MalInt Integer
    | MalString Text
    | MalCall MalFn MalAst

data MalFn
    = MalQuote
    | MalQuasiquote
    | MalUnquote
    | MalSpliceUnquote
    | MalDeref
    | MalWithMeta MalAst

instance Show MalFn where
    show MalQuote = "quote"
    show MalQuasiquote = "quasiquote"
    show MalUnquote = "unquote"
    show MalSpliceUnquote = "splice-unquote"
    show MalDeref = "deref"
    show (MalWithMeta _) = "with-meta"

instance Show MalAst where
    show (MalList l) = "(" <> unwords (map show l) <> ")"
    show (MalVector l) = "[" <> unwords (map show l) <> "]"
    show (MalMap kvs) = "{" <> unwords (map show $ toList kvs) <> "}"
      where
        toList = concatMap pairToList
        pairToList (a, b) = [a, b]
    show (MalSymbol s) = unpack s
    show (MalInt i) = show i
    show (MalString s) = show s
    show (MalCall (MalWithMeta metaAst) ast) = "(with-meta " <> show ast <> " " <> show metaAst <> ")"
    show (MalCall fn ast) = "(" <> show fn <> " " <> show ast <> ")"
    show (MalKeyword s) = ":" <> unpack s

isMalSpace :: Char -> Bool
isMalSpace c = isSpace c || c == ','

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 lineComment blockComment
  where
    space1 = void $ M.takeWhile1P (Just "white space") isMalSpace

    lineComment = L.skipLineComment ";"
    blockComment = L.skipBlockComment "#_" "_#"

stringLiteral :: Parser Text
stringLiteral = pack <$> (M.char '"' *> manyTill L.charLiteral (M.char '"'))

integer :: Parser Integer
integer = lexeme L.decimal

symbolParser :: Parser Text
symbolParser = pack <$> many (M.satisfy (\c -> not (isMalSpace c) && c `notElem` ['(', ')', '[', ']', '"', ';', '@', '~', '`', ',', '{', '}', ':', '#', '^', '"']))

malParser :: Parser MalAst
malParser = do
    _ <- M.many M.space1
    parser
  where
    parser =
        lexeme $
            choice
                [ do
                    fn <- fnParser
                    MalCall fn <$> parser
                , MalList <$> listParser
                , MalMap <$> mapParser
                , MalVector <$> vectorParser
                , MalInt <$> L.decimal
                , MalString <$> stringLiteral
                , MalKeyword <$> keywordParser
                , MalSymbol <$> symbolParser
                ]

    keywordParser :: Parser Text
    keywordParser = M.char ':' >> symbolParser

    fnParser :: Parser MalFn
    fnParser =
        choice
            [ MalQuote <$ M.string "'"
            , MalQuasiquote <$ M.string "`"
            , MalSpliceUnquote <$ M.try (M.string "~@")
            , MalUnquote <$ M.string "~"
            , MalDeref <$ M.string "@"
            , MalWithMeta <$> (M.string "^" *> parser)
            ]

    listParser =
        lexeme (M.char '(')
            *> manyTill
                ( do
                    ast <- parser
                    M.notFollowedBy M.eof
                    pure ast
                )
                ( do
                    _ <- M.char ')'
                    pure ()
                )
    vectorParser =
        lexeme (M.char '[')
            *> manyTill
                ( do
                    ast <- parser
                    M.notFollowedBy M.eof
                    pure ast
                )
                ( do
                    _ <- M.char ']'
                    pure ()
                )

    mapParser =
        lexeme (M.char '{')
            *> manyTill
                ( do
                    keyAst <- parser
                    M.notFollowedBy M.eof
                    M.notFollowedBy (M.char '}')
                    valAst <- parser
                    M.notFollowedBy M.eof
                    pure (keyAst, valAst)
                )
                ( do
                    _ <- M.char '}'
                    pure ()
                )

read :: IO (Either (M.ParseErrorBundle Text Void) MalAst)
read = do
    putStrLn "user> "
    parse malParser "" <$> getLine

eval :: MalAst -> MalAst
eval s = s

print :: MalAst -> IO ()
print ast = putStrLn $ pack $ show ast

step1_read_print :: IO ()
step1_read_print = do
    input <- read
    case input of
        Right ast -> print $ eval ast
        Left err -> putStrLn $ pack $ M.errorBundlePretty err
    step1_read_print
