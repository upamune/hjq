{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Query where

import Control.Applicative
import Control.Monad (join)
import Control.Lens.Operators ((^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Attoparsec.Text
import Data.Hjq.Parser
import Data.Semigroup ((<>))
import Data.Text

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

applyFilter :: JqFilter -> Value -> Either Text Value
applyFilter (JqField fieldName n) obj@(Object _) = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) array@(Array _) = join $ noteOutOfRangeError index (fmap (applyFilter n) (array ^? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> ":" <> tshow o

noteNotFoundError :: Text -> Maybe a -> Either Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "field name not found" <> s

noteOutOfRangeError :: Int -> Maybe a -> Either Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range: " <> tshow s

tshow :: Show a => a -> Text
tshow = pack . show

executeQuery :: JqQuery -> Value -> Either Text Value
executeQuery (JqQueryObject o) v = fmap (Object . H.fromList) . sequence . fmap sequence $ fmap (fmap $ flip executeQuery v) o
executeQuery (JqQueryArray l) v = fmap (Array . V.fromList) . sequence $ fmap (flip executeQuery v) l
executeQuery (JqQueryFilter f) v = applyFilter f v
