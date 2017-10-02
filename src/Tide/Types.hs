{-# LANGUAGE DeriveGeneric #-}

module Tide.Types where

import Linear
import Data.Binary
import GHC.Generics


data CommandType
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  deriving (Eq, Show, Generic)

instance Binary CommandType

type PlayerInput = (CommandType, Bool)

data DisplayData = DisplayData (V3 Double) (V3 Double) [V3 Double]
  deriving (Eq, Show, Generic)

instance Binary DisplayData

type UserName = String

data UserInfo =
  UserInfo UserName
           String
  deriving (Eq, Show, Generic)

instance Binary UserInfo
