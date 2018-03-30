module Data.Hjq where

import Data.Text

import Data.Hjq.Parser

parseJqFilter :: Text -> Either String JqFilter
parseJqFilter _ = Right JqNil
