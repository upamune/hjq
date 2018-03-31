{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter -- Either String JqFilter だったのをTextにしたが大丈夫か?
parseJqFilter s = showParseResult $ parse (jqFilterParser <* endOfInput) s `feed` ""

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
    where
        jqFilter :: Parser JqFilter
        jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

        jqField :: Parser JqFilter
        jqField = JqField <$> (word <* skipSpace) <*> jqFilter

        jqIndex :: Parser JqFilter
        jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

word :: Parser Text
word = fmap pack $ many1 (letter <|> schar '-' <|> schar '_' <|> digit)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace