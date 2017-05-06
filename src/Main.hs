module Main where

import Imports
import Page
import Types
import Sitemap

import qualified Dhall

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.PDF
import Data.Time.Clock
import Data.Time.Calendar

main :: IO ()
main = do
  posts <- filterM (doesFileExist . ("posts/"++)) =<< listDirectory "posts/"
  entries <- forM posts $ \p -> do
    ent <- Dhall.detailed $ Dhall.input
      (Dhall.auto :: Dhall.Type (Entry TL.Text DhallDay))
      (TL.pack $ "./posts/" ++ p)
    let entry = ent {
          entryCreated = (\DhallDay{..} -> fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) (entryCreated ent)
        , entryUpdated = (\DhallDay{..} -> fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) (entryUpdated ent)
        }
    case (readMarkdown def (TL.unpack $ entryAbstract entry), readMarkdown def (TL.unpack $ entryContent entry)) of
      (Right p1, Right p2) -> pure $ entry {entryAbstract = p1, entryContent = p2}
      (Left p1, _) -> fail $ "Encountered pandoc error: " <> show p1
      (_, Left p2) -> fail $ "Encountered pandoc error: " <> show p2

  case addEntryURLs entries of
    Left e -> do
      fail $ "Couldn't create a unique url for entry:" <> show e
    Right en -> writeFiles en

writeFiles :: [(Text, Entry Pandoc Day)] -> IO ()
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

  today <- utctDay <$> getCurrentTime
  BL.writeFile "sitemap.xml" $ createSitemap $ (concat :: [[a]] -> [a])
    [ flip map entries $ \(url, Entry{..}) ->
        PageData
          { pageLocation = url
          , pageLastMod = entryUpdated
          , pageType = Article entryImportance
          }
    , flip map entries $ \(url, Entry{..}) ->
        PageData
          { pageLocation = T.pack $ T.unpack url -<.> ".pdf"
          , pageLastMod = entryUpdated
          , pageType = Article entryImportance
          }
    , flip map ([minBound .. maxBound] :: [Keyword]) $ \k -> do
        PageData
          { pageLocation = displayUrl k
          , pageLastMod = maximum
              $ (addDays (-1000) today:)
              $ map (entryUpdated . snd) $ filter ((k `elem`) . entryKeywords . snd) $ entries
          , pageType = FrontPage
          }
    , (:[]) $ PageData
      { pageLocation = "index.html"

      -- there is only a recompilation, if something changed.
      , pageLastMod = today
      , pageType = FrontPage
      }
    ]
  where
    pdfOptions e tmpl = def
      { writerHighlight = True
      , writerTemplate = Just $ T.unpack tmpl
      , writerVariables =
        [ ("title", TL.unpack $ entryTitle e)
        , ("author", "Anton Felix Lorenzen")
        , ("documentclass", "article")
        , ("papersize", "a4")
        , ("fontsize", "12pt")
        , ("linestretch", "1.5")
        , ("geometry", "margin=3cm")
        ]
      }
