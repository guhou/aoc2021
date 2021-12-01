module AOC2021.Day1
  ( Day1Options (..),
    day1Options,
    runDay1,
  )
where

import Conduit
  ( ConduitT,
    MonadTrans (lift),
    PrimMonad,
    ResourceT,
    Void,
    decodeUtf8C,
    lengthIfC,
    linesUnboundedC,
    mapC,
    mapMC,
    runConduit,
    runConduitRes,
    runResourceT,
    slidingWindowC,
    sourceFile,
    stdinC,
    (.|),
  )
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as U
import Options.Applicative
  ( Parser,
    auto,
    help,
    long,
    metavar,
    option,
    short,
    showDefault,
    strOption,
    value,
  )
import TextShow (printT)

data Day1Options = Day1Options
  { day1FilePath :: FilePath,
    day1Window :: Int
  }
  deriving (Eq, Ord, Read, Show)

day1Options :: Parser Day1Options
day1Options = Day1Options <$> filePath <*> slidingWindow
  where
    filePath :: Parser FilePath
    filePath =
      strOption
        ( long "file"
            <> short 'f'
            <> metavar "FILE"
            <> help "Input file"
            <> value "-"
        )
    slidingWindow :: Parser Int
    slidingWindow =
      option
        auto
        ( long "window"
            <> short 'w'
            <> metavar "WINDOW"
            <> help "How many measurements should be observed in each window"
            <> showDefault
            <> value 1
        )

runDay1 :: Day1Options -> IO ()
runDay1 Day1Options {..} = do
  count <-
    runConduitRes $
      readInput day1FilePath
        .| readMeasurements
        .| windowMeasurements day1Window
        .| countIncreases
  printT count

readInput :: FilePath -> ConduitT () Text (ResourceT IO) ()
readInput path =
  let rawInput =
        if path == "-"
          then stdinC
          else sourceFile path
   in rawInput .| decodeUtf8C

readMeasurements :: (MonadFail m, PrimMonad m) => ConduitT Text Int m ()
readMeasurements = linesUnboundedC .| mapMC readMeasurement
  where
    readMeasurement = either fail (pure . fst) . decimal

windowMeasurements :: (Monad m) => Int -> ConduitT Int Int m ()
windowMeasurements windowSize = slidingWindowC windowSize .| mapC U.sum

countIncreases :: (Monad m) => ConduitT Int Void m Int
countIncreases = slidingWindowC 2 .| lengthIfC isIncrease
  where
    isIncrease :: U.Vector Int -> Bool
    isIncrease w = w ! 0 < w ! 1
