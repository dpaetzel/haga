{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Options.Applicative
import Pipes
import Pretty
import Protolude hiding (for)
import System.IO
import Seminar
import Szenario191

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
    let seminarEE = prios
    let env = AssignmentEnviroment (students seminarEE, topics seminarEE)
    let selType = Tournament 3
    let run' = run seminarEE env selType 120 (5 / 100) (populationSize opts) (steps (iterations opts))
    pop' <- runEffect (for run' logCsv)
    seminarEE' <- calc seminarEE  pop'
    let (res, _) = bests seminarEE' 5 pop'
    seminarEE' <- calc seminarEE' res
    mapM_ (format seminarEE') res
  where
    format seminarL s = do
      let f = fitness' seminarL s
      putErrText $ show f <> "\n" <> pretty s
    logCsv = putText . csv
    csv (t, f) = show t <> " " <> show f
