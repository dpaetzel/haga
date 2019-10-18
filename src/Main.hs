{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude hiding (for)
import Pretty
import WS19
import Pipes
import System.IO

mkPop = population 100 (I prios [])

main :: IO ()
main = do
  args <- getArgs
  let t = fromMaybe 100 $ headMay args >>= readMaybe
  hSetBuffering stdout NoBuffering
  pop <- mkPop
  pop' <- runEffect $ for (run 2 1 pop (steps t)) log
  res <- bests 5 pop'
  sequence_ $ format <$> res
  where
    format s = do
      f <- liftIO $ fitness s
      putErrText $ show f <> "\n" <> pretty s
    log = putText . csv
    csv (t, f) = show t <> " " <> maybe "inf" show f
