module Main where

import Imports
import qualified Entries
import Page
import Types

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Text.Pandoc.PDF

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

  BL.writeFile "index.html"
    $ renderFrontPage Nothing
    $ entriesToHeadline entries

  let keys = [minBound .. maxBound] :: [Keyword]
  forM_ keys $ \k -> do
    let name = T.unpack $ displayUrl k
    let entr = filter ((k `elem`) . entryKeywords . snd) $ entries
    createDirectoryIfMissing False name
    BL.writeFile (name </> "index.html")
      $ renderFrontPage (Just k)
      $ entriesToHeadline entr

  latexTemplate <- readFile "../css/template.tex"
  forM_ entries $ \(u, e) -> do
    createDirectoryIfMissing False "posts"
    BL.writeFile (T.unpack u) $ renderPage (u, e)
    pdf <- makePDF "xelatex" writeLaTeX (pdfOptions e latexTemplate) (entryContent e)
    case pdf of
      Left b -> putStr ("Error while creating pdf: " <> b)
      Right b -> BL.writeFile (T.unpack u -<.> ".pdf") b

  where
    pdfOptions e tmpl = def
      { writerHighlight = True
      , writerTemplate = Just $ T.unpack tmpl
      , writerVariables =
        [ ("title", T.unpack $ entryTitle e)
        , ("author", "Anton Felix Lorenzen")
        , ("documentclass", "article")
        , ("papersize", "a4")
        , ("fontsize", "12pt")
        , ("linestretch", "1.5")
        , ("geometry", "margin=3cm")
        ]
      }
