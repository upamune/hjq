module Data.Hjq.Parser where

import Data.Text

data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)
