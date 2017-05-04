module Types where

import Imports


data Category
  = Haskell
  | ReadingList
  | ShortStories
  deriving (Eq, Bounded, Enum, Show)

instance Display Category where
  displayTitle Haskell = "Haskell"
  displayTitle ReadingList = "My reading list"
  displayTitle ShortStories = "Short stories"

  displayDescription Haskell = "Haskell is a functional programming language; I try to explain it's concepts."
  displayDescription ReadingList = "I set myself a goal of reading a book a week and write a small entry about each."
  displayDescription ShortStories = "I am writing short stories about places, people and things, that left an impression."


data Language
  = English
  | Deutsch
  deriving (Eq, Bounded, Enum, Show)

newtype Comments
 = Reddit Text
 deriving (Eq, Show)


data Keyword
 = Blogging
 deriving (Eq, Bounded, Enum, Show)

instance Display Keyword where
