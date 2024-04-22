{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaDatasets.NurseryDefinition
  ( module LambdaDatasets.NurseryDefinition,
    module CommonDefinition,
  ) where

import Protolude
import CommonDefinition

data NurseryClass = NotRecommend | Recommend | VeryRecommend | Priority | SpecPriority deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Parents = Usual | Pretentious | GreatPret deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data HasNurs = ProperNurs | LessProperNurs | ImproperNurs | CriticalNurs | VeryCritNurs deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Form    = CompleteFamilyForm | CompletedFamilyForm | IncompleteFamilyForm | FosterFamilyForm deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Children = OneChild | TwoChilds | ThreeChilds | MoreChilds deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Housing = ConvenientHousing | LessConvHousing | CriticalHousing deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Finance = ConvenientFinance | InconvFinance deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Social = NotProblematicSocial | SlightlyProblematicSocial | ProblematicSocial deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Health = NotRecommendHealth |RecommendedHealth | PriorityHealth  deriving (Eq, Generic, Show, Enum, Bounded, Ord)

