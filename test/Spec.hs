{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Hjq
import Data.Hjq.Parser

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ jqFilterParserTest ]
    return ()

jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [ "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil ]