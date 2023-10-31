-- |
-- Module      : Main
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
module Main where

import WithCli (HasArguments(..), withCli)
import Parser.Text (genModules)
import GHC.Generics (Generic)

data CLIOptions =
    CLIOptions
        { input :: String
        , output :: String
        }
    deriving (Show, Generic, HasArguments)

cliClient :: CLIOptions -> IO ()
cliClient opts = genModules (input opts) (output opts)

main :: IO ()
main = withCli cliClient
