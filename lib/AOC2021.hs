module AOC2021 (main) where

{- HLINT ignore "Use newtype instead of data" -}

import AOC2021.Day1 (Day1Options, day1Options, runDay1)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList, (!?))
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    execParser,
    fullDesc,
    header,
    helper,
    hsubparser,
    info,
    long,
    progDesc,
    switch,
    (<**>),
  )
import System.Environment (getArgs)

data AOCOptions = AOCOptions
  { aocCommand :: Command
  }
  deriving (Eq, Ord, Read, Show)

aocInfo :: ParserInfo AOCOptions
aocInfo =
  info
    (aocOptions <**> helper)
    ( fullDesc
        <> progDesc "Advent of Code 2021"
        <> header "aoc2021: @guhou's solutions to Advent of Code 2021"
    )

aocOptions :: Parser AOCOptions
aocOptions =
  AOCOptions
    <$> hsubparser
      ( command "day1" (Day1Command <$> info day1Options (progDesc "Run solution for day 1"))
      )

data Command
  = Day1Command Day1Options
  deriving (Eq, Ord, Read, Show)

main :: IO ()
main = do
  options <- execParser aocInfo
  runAOC options

runAOC :: AOCOptions -> IO ()
runAOC AOCOptions {..} =
  case aocCommand of
    Day1Command opts -> runDay1 opts
