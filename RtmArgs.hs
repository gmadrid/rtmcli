{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RtmArgs (Options,
                optCommands,
                optDumpJson,
                optRefreshToken,
                parseOpts
               ) where

import ClassyPrelude
import Data.Default
import System.Console.GetOpt
import System.Environment

data Options = Options { optCommands :: [Text],
                         optDumpJson :: Bool,
                         optRefreshToken :: Bool
                       }
instance Default Options where
  def = Options {
    optCommands = [],
    optDumpJson = False,
    optRefreshToken = False
    }

options :: [ OptDescr (Options -> Options) ]
options = [ Option "e" []
            (ReqArg (\e opts -> let te = fromString e in
                      opts { optCommands = optCommands opts ++ [te] })
             "CMD")
            "Execute this command and exit.",

            Option [] ["dump_json"]
            (NoArg (\o -> o { optDumpJson = True }))
            "Dump text of JSON responses.",

            Option [] ["refresh_token"]
            (NoArg (\o -> o { optRefreshToken = True }))
            "Force a token refresh."
          ]

parseOpts :: IO (Options, [Text])
parseOpts = do
  argv <- System.Environment.getArgs
  case getOpt Permute options argv of
   (o,n,[]  ) -> return (foldl' (flip id) def o, fmap fromString n)
   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: favwrite [OPTION...]"
