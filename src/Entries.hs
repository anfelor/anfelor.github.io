module Entries
  ( entries
  ) where

import Imports
import NewEntry
import Text.Pandoc

import qualified Entries.E170426

go :: Entry (Either PandocError Pandoc) -> Either (Entry (Either PandocError Pandoc)) (Entry Pandoc)
go x = case (entryContent x, entryAbstract x) of
  (Left _, _) -> Left x
  (_, Left _) -> Left x
  (Right p, Right q) -> Right $ x {entryContent = p, entryAbstract = q}

entries :: Either (Entry (Either PandocError Pandoc)) [Entry Pandoc]
entries = sequence $ fmap go
  [ Entries.E170426.entry
  ]
