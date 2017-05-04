module Main where

import Imports
import qualified Entries
import Page
import Types
import NewEntry

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Text.Pandoc

main :: IO ()
main = do
  case Entries.entries of
    Left pe -> do
      putText "Couldn't parse the content of entry:"
      print pe
    Right en -> case addEntryURLs en of
      Left e -> do
        putText "Couldn't create a unique url for entry:"
        print e
      Right entries -> writeFiles entries

writeFiles :: [(Text, Entry Pandoc)] -> IO ()
writeFiles entries = do
  removePathForcibly "blog"
  createDirectory "blog"
  setCurrentDirectory "blog"

  BL.writeFile "imprint.html"
    $ renderImprint

  BL.writeFile "privacy.html"
    $ renderPrivacy

  BL.writeFile "index.html"
    $ renderFrontPage (Nothing :: Maybe Category)
    $ entriesToHeadline entries

  let cats = [minBound .. maxBound] :: [Category]
  forM_ cats $ \c -> do
    let name = T.unpack $ displayUrl c
    let entr = filterEntries c entries
    createDirectoryIfMissing False name
    BL.writeFile (name </> "index.html")
      $ renderFrontPage (Just c)
      $ entriesToHeadline entr

  let keys = [minBound .. maxBound] :: [Keyword]
  forM_ keys $ \k -> do
    let name = T.unpack $ displayUrl k
    let entr = filterEntries k entries
    createDirectoryIfMissing False name
    BL.writeFile (name </> "index.html")
      $ renderFrontPage (Just k)
      $ entriesToHeadline entr

  forM_ entries $ \(u, e) -> do
    createDirectoryIfMissing False "posts"
    BL.writeFile (T.unpack u) $ renderPage e

class (Bounded a, Enum a, Display a) => FrontpageItem a where
  filterEntries :: a -> [(b, Entry c)] -> [(b, Entry c)]

instance FrontpageItem Category where
  filterEntries c = filter ((c`elem`) . entryCategory . snd)

instance FrontpageItem Keyword where
  filterEntries k = filter ((k`elem`) . entryKeywords . snd)
