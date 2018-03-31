{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Hjq
import Data.Hjq.Parser

main :: IO ()
main = do
    _ <- runTestTT $ TestList
        [ jqFilterParserTest
        , jqFilterParserSpaceTest ]
    return ()

jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [ "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
    , "jqFilterParser test 2" ~: parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser test 3" ~: parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~: parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser test 5" ~: parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqFilterParserSpaceTest :: Test
jqFilterParserSpaceTest = TestList
    [ "jqFilterParser space test 1" ~: parseJqFilter ". " ~?= Right JqNil
    , "jqFilterParser space test 2" ~: parseJqFilter ".[ 0 ]" ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParser space test 3" ~: parseJqFilter " .fieldName " ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser space test 4" ~: parseJqFilter " .[ 0 ].fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParser space test 5" ~: parseJqFilter " .fieldName[ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]
