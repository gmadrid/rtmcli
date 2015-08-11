{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmArgs (Options,
                optRefreshToken,
                parseOpts
               ) where

import ClassyPrelude
import System.Console.GetOpt
import System.Environment

data Options = Options { optRefreshToken :: Bool }

defaultOptions = Options {
  optRefreshToken = False
  }

options :: [ OptDescr (Options -> Options) ]
options = [ Option [] ["refresh_token"]
            (NoArg (\o -> o { optRefreshToken = True }))
            "Force a token refresh."
          ]

parseOpts :: IO (Options, [Text])
parseOpts = do
  argv <- System.Environment.getArgs
  case getOpt Permute options argv of
   (o,n,[]  ) -> return (foldl' (flip id) defaultOptions o, fmap fromString n)
   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: favwrite [OPTION...]"

