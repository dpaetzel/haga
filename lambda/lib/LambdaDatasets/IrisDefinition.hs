{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaDatasets.IrisDefinition
  ( module LambdaDatasets.IrisDefinition,
    module CommonDefinition,
  ) where

import Protolude
import CommonDefinition

data IrisClass = Setosa | Virginica | Versicolor deriving (Eq, Generic, Show, Enum, Bounded)

