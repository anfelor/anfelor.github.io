module Types where

import Imports


data Category
  = Haskell
  | Kurzgeschichten
  | ReadingList
  deriving (Eq, Bounded, Enum, Show)

instance Display Category where
  display ReadingList = "Reading list"
  display a = fromString $ show a


data Language
  = English
  | Deutsch
  deriving (Eq, Bounded, Enum, Show)

instance Display Language


newtype Comments
 = Reddit Text
 deriving (Eq, Show)


data Keyword
 = Blogging
 deriving (Eq, Bounded, Enum, Show)

instance Display Keyword where
