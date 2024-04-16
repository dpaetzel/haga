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
import GermanDataset
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
          <> value 1000
          <> help "Number of iterations"
      )
    <*> option
      auto
      ( long "population-size"
          <> short 'p'
          <> metavar "N"
          <> value 100
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
    germanLEE <- shuffledGermanLEE
    let env = germanLE
    let selType = Tournament 3
    let run' = run germanLEE env selType 80 (5 / 100) (populationSize opts) (steps (iterations opts))
    pop' <- runEffect (for run' logCsv)
    germanLEE' <- calc germanLEE  pop'
    let (res, _) = bests germanLEE' 5 pop'
    let germanLEE' = germanLEE {training = False}
    germanLEE' <- calc germanLEE' res
    mapM_ (format germanLEE') res
  where
    format germanL s = do
      let f = fitness' germanL s
      putErrText $ show f <> "\n" <> pretty s
    logCsv = putText . csv
    csv (t, f) = show t <> " " <> show f
