{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Options.Applicative
import Pipes
import Pretty
import Protolude hiding (for, option)
import System.IO
-- import Szenario212Pun
import Szenario222

data Options = Options
  { iterations :: N,
    populationSize :: N
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
    pop <- population (populationSize opts) (I prios [])
    pop' <-
      runEffect $
        for (run (tournament 2) 2 1 (5 / 100) pop (steps $ iterations opts)) log
    (res, _) <- bests 5 pop'
    sequence_ $ format <$> res
  where
    format s = do
      f <- liftIO $ fitness s
      putErrText $ show f <> "\n" <> pretty s
    log = putText . csv
    csv (t, f) = show t <> " " <> show f
