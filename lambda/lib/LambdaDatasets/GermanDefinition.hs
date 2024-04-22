{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LambdaDatasets.GermanDefinition
  ( module LambdaDatasets.GermanDefinition,
    module CommonDefinition,
  ) where


import Protolude
import CommonDefinition

data GermanClass = Accept | Deny deriving (Eq, Generic, Show, Enum, Bounded)

data AccountStatus = AccountInDebt | NoAccount | LowAccountBalance | HighAccountBalanceOrRegular deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data CreditHistory = HistoryGood | HistoryGoodHere | HistoryGoodSoFar | DelaysInHistory | CreditsExist deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Purpose = OldCar | NewCar | FunitureOrEquipment | Tech | Appliances | Repairs | Education | Retraining | Business | Other deriving (Eq, Generic, Show, Enum, Bounded)

data Savings = UnknownOrNone | SmallSavings | NormalSavings | GoodSavings | GreatSavings deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data EmploymentStatus = NotEmployed | ShortTermEmployed | MediumTermEmployed | LongTermEmployed | VeteranEmployed deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data StatusAndSex = MaleAndSeperated | FemaleAndSeperatedOrMarried | MaleAndSingle | FemaleAndSingle | MaleAndWidowedOrMarried deriving (Eq, Generic, Show, Enum, Bounded)

data OtherDebtors = NoOtherDebtors | CoApplicant | Guarantor deriving (Eq, Generic, Show, Enum, Bounded, Ord)

data Property = UnknownOrNoProperty | RealEstate | Savings | CarOrOther deriving (Eq, Generic, Show, Enum, Bounded)

data OtherPlans = PlansAtBank | PlansAtStores | NoOtherPlans deriving (Eq, Generic, Show, Enum, Bounded)

data Housing = Renting | OwningRecidency | ResidingForFree deriving (Eq, Generic, Show, Enum, Bounded)

data Job = UnemployedOrUnskilledNonResident | UnskilledResident | Skilled | HighlySkilled deriving (Eq, Generic, Show, Enum, Bounded, Ord)
