{-# LANGUAGE NoImplicitPrelude #-}

module CommonDefinition where

import Protolude

if' :: Bool -> a -> a -> a
if' True e _ = e
if' False _ e = e
