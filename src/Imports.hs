module Imports
  ( module Exports
  , module Imports
  ) where

import Protolude as Exports hiding ((<.>))

import Data.Char as Exports
import Data.List.Split as Exports
import Data.List.NonEmpty as Exports (NonEmpty)
import Data.String as Exports
import Data.Text as Exports (Text)
import Data.Time as Exports (Day, fromGregorian)

import System.Directory as Exports
import System.FilePath as Exports

import qualified Data.Text as T

class Show a => Display a where
  display :: a -> Text
  display = fromString . show

-- | Turn the showable into a string, splitting it into it's word parts
-- and joining them with a dash '-'. The result is all lower case.
urlFormat :: Display a => a -> Text
urlFormat a = case T.uncons (show a) of
  Just (h, t) -> T.foldl' go (T.singleton (toLower h)) t
  Nothing -> undefined -- show is guaranteed to return non-empty text.
  where
    go :: Text -> Char -> Text
    go txt c = if isUpper c
      then txt <> "-" <> T.singleton (toLower c)
      else T.snoc txt c
