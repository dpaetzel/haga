{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Options.Applicative
import Pipes
import Pretty
import Protolude hiding (for)
import System.IO
-- import Szenario212Pun
-- import Szenario191
import IrisDataset

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
          <> value 1000
          <> help "Number of iterations"
      )
    <*> option
      auto
      ( long "population-size"
          <> short 'p'
          <> metavar "N"
          <> value 50
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
    let env = irisLE
    let selType = Tournament 3
    let run' = run irisLEE env selType 40 (5 / 100) (populationSize opts) (steps (iterations opts))
    pop' <-
      runEffect (for run' logCsv)

    irisLE <- calc irisLEE  pop'
    let (res, _) = bests irisLE 5 pop'
    mapM_ (format irisLE) res
  where
    format irisL s = do
      let f = fitness' irisL s
      putErrText $ show f <> "\n" <> pretty s
    logCsv = putText . csv
    csv (t, f) = show t <> " " <> show f
