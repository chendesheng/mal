{-# LANGUAGE OverloadedStrings #-}

module Step2 where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStrLn)
import Data.Void (Void)
import Debug.Trace (traceShow)
import Text.Megaparsec (Parsec, choice, many, manyTill, parse)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (getLine, print, putStrLn, read)

type Parser = Parsec Void Text

data MalVal
    = MalList [MalVal]
    | MalVector [MalVal]
    | MalMap [(MalVal, MalVal)]
    | MalSymbol Text
    | MalKeyword Text
    | MalInt Integer
    | MalString Text
    | MalCall MalFn MalVal

data MalFn
    = MalQuote
    | MalQuasiquote
    | MalUnquote
    | MalSpliceUnquote
    | MalDeref
    | MalWithMeta MalVal

instance Show MalFn where
    show MalQuote = "quote"
    show MalQuasiquote = "quasiquote"
    show MalUnquote = "unquote"
    show MalSpliceUnquote = "splice-unquote"
    show MalDeref = "deref"
    show (MalWithMeta _) = "with-meta"

instance Show MalVal where
    show (MalList l) = "(" <> unwords (map show l) <> ")"
    show (MalVector l) = "[" <> unwords (map show l) <> "]"
    show (MalMap kvs) = "{" <> unwords (map show $ toList kvs) <> "}"
      where
        pairToList (a, b) = [a, b]
        toList = concatMap pairToList
    show (MalSymbol s) = unpack s
    show (MalInt i) = show i
    show (MalString s) = show s
    show (MalCall (MalWithMeta metaAst) ast) = "(with-meta " <> show ast <> " " <> show metaAst <> ")"
    show (MalCall fn ast) = "(" <> show fn <> " " <> show ast <> ")"
    show (MalKeyword s) = ":" <> unpack s

isMalSpace :: Char -> Bool
isMalSpace c = isSpace c || c == ','

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    space1 = void $ M.takeWhile1P (Just "white space") isMalSpace

    lineComment = L.skipLineComment ";"
    blockComment = L.skipBlockComment "#_" "_#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser Text
stringLiteral = pack <$> (M.char '"' *> manyTill L.charLiteral (M.char '"'))

symbolParser :: Parser Text
symbolParser = pack <$> many (M.satisfy (\c -> not (isMalSpace c) && c `notElem` ['(', ')', '[', ']', '"', ';', '@', '~', '`', ',', '{', '}', ':', '#', '^', '"']))

malParser :: Parser MalVal
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
                , MalInt <$> M.try intParser
                , MalString <$> stringLiteral
                , MalKeyword <$> keywordParser
                , MalSymbol <$> symbolParser
                ]

    intParser :: Parser Integer
    intParser = do
        choice
            [ do
                _ <- M.try $ M.char '-'
                i <- L.decimal
                pure $ -i
            , L.decimal
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

read :: IO (Either (M.ParseErrorBundle Text Void) MalVal)
read = do
    putStrLn "user> "
    parse malParser "" <$> getLine

eval :: MalVal -> Either String MalVal
eval = go
  where
    go :: MalVal -> Either String MalVal
    go (MalList (MalSymbol name : params)) =
        case lookup name libs of
            Just f ->
                case f <$> mapM go params of
                    Left err -> Left err
                    Right n -> MalInt <$> n
            Nothing -> Right $ MalSymbol name
    go (MalVector l) = MalVector <$> mapM go l
    go (MalMap l) =
        MalMap
            <$> mapM
                ( \(a, b) -> do
                    a' <- go a
                    b' <- go b
                    pure (a', b')
                )
                l
    go x = Right x

libs :: [(Text, [MalVal] -> Either String Integer)]
libs =
    [ ("+", malAdd)
    , ("-", malSub)
    , ("*", malMul)
    , ("/", malDiv)
    ]
  where
    malAdd :: [MalVal] -> Either String Integer
    malAdd [] = Left "Invalid number of arguments"
    malAdd [MalInt a] = Right a
    malAdd (MalInt a : xs) =
        case malAdd xs of
            Right b -> Right $ a + b
            Left err -> Left err
    malAdd _ =
        Left "Invalid argument type"

    malSub :: [MalVal] -> Either String Integer
    malSub [] = Left "Invalid number of arguments"
    malSub [MalInt a] = Right a
    malSub (MalInt a : xs) =
        case malSub xs of
            Right b -> Right $ a - b
            Left err -> Left err
    malSub _ =
        Left "Invalid argument type"

    malMul :: [MalVal] -> Either String Integer
    malMul [] = Left "Invalid number of arguments"
    malMul [MalInt a] = Right a
    malMul (MalInt a : xs) =
        case malMul xs of
            Right b -> Right $ a * b
            Left err -> Left err
    malMul _ =
        Left "Invalid argument type"

    malDiv :: [MalVal] -> Either String Integer
    malDiv [] = Left "Invalid number of arguments"
    malDiv [MalInt a] = Right a
    malDiv (MalInt a : xs) =
        case malDiv xs of
            Right b -> Right $ a `div` b
            Left err -> Left err
    malDiv _ =
        Left "Invalid argument type"

print :: MalVal -> IO ()
print ast = putStrLn $ pack $ show ast

step2_eval :: IO ()
step2_eval = do
    input <- read
    case input of
        Right ast ->
            case eval ast of
                Left err -> putStrLn $ pack err
                Right ast' -> print ast'
        Left err -> putStrLn $ pack $ M.errorBundlePretty err
    step2_eval
