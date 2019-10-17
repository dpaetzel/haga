{-# LANGUAGE NoImplicitPrelude #-}


import Protolude


import GA
import SS19


main = do
  pop <- mkPop
  ga' 2 1 pop (\_ t -> t > 100) 10
  putStrLn "Done."


mkPop = population 100 (I prios [])
