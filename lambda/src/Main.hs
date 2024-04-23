{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Options.Applicative
import Pipes
import Pretty
import Protolude hiding (for)
import System.IO
-- import LambdaDatasets.IrisDataset
import LambdaDatasets.NurseryDataset
-- import LambdaDatasets.GermanDataset
import Debug.Trace as DB
import qualified Data.Map.Strict as Map

data Options = Options
  { iterations :: !N,
    populationSize :: !N
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "iterations"
          <> short 'i'
          <> metavar "N"
          <> value 1500
          <> help "Number of iterations"
      )
    <*> option
      auto
      ( long "population-size"
          <> short 'p'
          <> metavar "N"
          <> value 400
          <> help "Population size"
      )

optionsWithHelp :: ParserInfo Options
optionsWithHelp =
  info
    (helper <*> options)
    ( fullDesc
        <> progDesc "Run a GA"
        <> header "haga - Haskell implementations of EAs"
    )

main :: IO ()
main =
  execParser optionsWithHelp >>= \opts -> do
    hSetBuffering stdout NoBuffering
    lEE <- shuffledLEE
    let env = lE
    let selType = Tournament 3
    let run' = run lEE env selType 120 (5 / 100) (populationSize opts) (steps (iterations opts))
    pop' <- runEffect (for run' logCsv)
    lEE' <- calc lEE  pop'
    let (res, _) = bests lEE' 5 pop'
    let lEE' = lEE {training = False}
    lEE' <- calc lEE' res
    mapM_ (format lEE') res
  where
    format l s = do
      let f = fitness' l s
      putErrText $ show f <> "\n" <> pretty s
    logCsv = putText . csv
    csv (t, f) = show t <> " " <> show f
