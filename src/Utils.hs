{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Map.Strict (fromList, Map)
import Data.Text       (Text)

import Database.Bolt as DB (Value)

type TemplateParams = Map Text DB.Value

class ToTemplateParams a where
  toTemplateParams :: a -> TemplateParams