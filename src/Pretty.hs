{-# LANGUAGE NoImplicitPrelude #-}

module Pretty where

import Protolude

class Pretty a where
  pretty :: a -> Text
