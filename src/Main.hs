{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude
import WS19

main = do
  pop <- mkPop
  ga' 2 1 pop (steps 10000) 10
  putText "Done."

mkPop = population 100 (I prios [])
