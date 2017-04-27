module NewEntry
  ( module NewEntry
  , module Exports
  ) where

import Imports as Exports (Text, Maybe(..), Bool(..), Day, fromGregorian)

import Types as Exports

import Imports
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Pandoc


data Entry a = Entry
  { entryTitle :: Text
  , entryCreated :: Day
  , entryUpdated :: Day
  , entryKeywords :: NonEmpty Keyword
  , entryCategory :: [Category]
  , entryLanguage :: Language
  , entryAbstract :: a
  , entryContent :: a
  , entryComments :: Maybe Comments
  } deriving (Show, Functor)

instance Eq (Entry a) where
  a == b = entryTitle a == entryTitle b
        && entryCreated a == entryCreated b

instance Ord (Entry a) where
  compare a b = case comparing entryCreated a b of
    LT -> LT
    GT -> GT
    EQ -> comparing entryTitle a b


md :: QuasiQuoter
md = QuasiQuoter
  { quoteExp = pure . AppE (AppE (VarE 'readMarkdown) (VarE 'def)) . LitE . stringL
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

rst :: QuasiQuoter
rst = QuasiQuoter
  { quoteExp = pure . AppE (AppE (VarE 'readRST) (VarE 'def)) . LitE . stringL
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

latex :: QuasiQuoter
latex = QuasiQuoter
  { quoteExp = pure . AppE (AppE (VarE 'readLaTeX) (VarE 'def)) . LitE . stringL
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
