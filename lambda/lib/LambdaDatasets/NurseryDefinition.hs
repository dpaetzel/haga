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
instance Hashable NurseryClass

data Parents = Usual | Pretentious | GreatPret deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Parents

data HasNurs = ProperNurs | LessProperNurs | ImproperNurs | CriticalNurs | VeryCritNurs deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable HasNurs

data Form    = CompleteFamilyForm | CompletedFamilyForm | IncompleteFamilyForm | FosterFamilyForm deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Form

data Children = OneChild | TwoChilds | ThreeChilds | MoreChilds deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Children

data Housing = ConvenientHousing | LessConvHousing | CriticalHousing deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Housing

data Finance = ConvenientFinance | InconvFinance deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Finance

data Social = NotProblematicSocial | SlightlyProblematicSocial | ProblematicSocial deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Social

data Health = NotRecommendHealth |RecommendedHealth | PriorityHealth  deriving (Eq, Generic, Show, Enum, Bounded, Ord)
instance Hashable Health
