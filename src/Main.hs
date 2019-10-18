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
  hSetBuffering stdout NoBuffering
  pop <- mkPop
  pop' <- runEffect $ for (run 2 1 pop (steps 100)) log
  res <- bests 5 pop
  sequence_ $ format <$> res
  where
    format :: (Individual i, MonadIO m, Pretty i) => i -> m ()
    format s = do
      f <- liftIO $ fitness s
      putErrText $ show f <> "\n" <> pretty s
    log = putText . csv
    csv (t, f) = show t <> " " <> maybe "inf" show f
