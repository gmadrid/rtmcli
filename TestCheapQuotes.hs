{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCheapQuotes (tests) where

import ClassyPrelude
import Test.HUnit

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H

import qualified CheapQuotes as CQ

basicTest :: (String, Test)
basicTest = (desc,
             TestCase $ assertEqual "" ["foo", "bar", "baz"] (CQ.words "foo bar baz")
            )
  where desc = "Basic case with no quotes"

simpleQuotes = (desc,
                TestCase $ assertEqual "" ["foobar", "baz"] (CQ.words "\"foo bar\" baz")
                )
               where desc = "Simple quotes at start/end of pieces"
             

tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [basicTest, simpleQuotes]

