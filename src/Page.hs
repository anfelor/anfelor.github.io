module Page where

import Imports hiding (head, link, div, map)
import NewEntry (Entry(..))
import Types

import Text.Pandoc
import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy as BL
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Data.HashSet as Set
import qualified Data.Text as T


data Headline = Headline
  { headlineTitle :: Text
  , headlineCreated :: Day
  , headlineUpdated :: Day
  , headlineCategory :: [Category]
  , headlineKeywords :: NonEmpty Keyword
  , headlineURL :: Text
  , headlineAbstract :: String
  } deriving (Eq, Show)

instance Ord Headline where
  compare h g = case comparing headlineCreated h g of
    LT -> LT
    GT -> GT
    EQ -> comparing headlineTitle h g


addEntryURLs :: [Entry a] -> Either (Entry a) [(Text, Entry a)]
addEntryURLs = fmap fst . flip runStateT Set.empty . mapM go . sort
  where
    go :: Entry a -> StateT (Set.HashSet Text) (Either (Entry a)) (Text, Entry a)
    go e = do
      used <- get
      let poss = fmap ((<>".html") . T.intercalate "-" . fmap urlFormat)
                   $ permutations $ toList $ entryKeywords e
      let choose = filter (`notElem` used) poss
      case choose of
        [] -> lift (Left e)
        (x:_) -> do
          put (Set.insert x used)
          pure (x, e)

entriesToHeadline :: [(Text, Entry Pandoc)] -> [Headline]
entriesToHeadline = fmap go
  where
    go (url, Entry {..}) = Headline
      { headlineTitle = entryTitle
      , headlineCreated = entryCreated
      , headlineUpdated = entryUpdated
      , headlineCategory = entryCategory
      , headlineKeywords = entryKeywords
      , headlineAbstract = writeHtmlString def entryAbstract
      , headlineURL = url
      }

standardPage :: Markup -> Markup -> Markup -> BL.ByteString
standardPage headPart sidebarPart mainPart = renderMarkup $ html $ do
  head $ do
    headPart
    link ! rel "stylesheet" ! type_ "text/css" ! href "/blog/css/main.css"
    link ! rel "stylesheet"
         ! href "https://fonts.googleapis.com/css?family=EB+Garamond|Raleway:400,700&amp;subset=latin-ext"
  body $ do
    div ! id "container" $ do
      div ! id "sidebar" $ do
        sidebarPart
      div ! id "main" $ do
        mainPart
        div ! class_ "fixend" $ pure ()


renderFrontPage :: (Enum a, Bounded a, Display a) => Maybe a -> [Headline] -> BL.ByteString
renderFrontPage ma headlines = standardPage
  (title categoryName)
  (div ! id "frontpage" $ do
    div ! id "lorenzen" $ do
      img ! id "personal-picture" ! src "/blog/img/DSCF0795_modified_small.JPG"
      h1 "Anton Felix Lorenzen"
    nav $ do
      ul $ forM_ ([minBound .. maxBound] :: [Category]) $ \c ->
        li $ a ! href (stringValue $ T.unpack $ urlFormat c <> "/") $ toMarkup $ display c)
  (ul $ do
    forM_ headlines $ \(Headline {..}) ->
      li $ do
        h2 $ do
          a ! href (stringValue $ T.unpack $ headlineURL) $ toMarkup $ headlineTitle
        p $ preEscapedString $ headlineAbstract)
  where
    categoryName = toMarkup $ maybe "" display ma


headersMinusTwo :: Pandoc -> Pandoc
headersMinusTwo = walk go
  where
    go :: Block -> Block
    go (Header n attr inl) = Header (n+2) attr inl
    go block = block

allHeaders :: Pandoc -> [(Int, Html)]
allHeaders pan@(Pandoc mt _)= query go pan
  where
    go :: Block -> [(Int, Html)]
    go (Header n _ inl) = [(n, writeHtml def (Pandoc mt [Para inl]))]
    go _ = []

renderPage :: Entry Pandoc -> BL.ByteString
renderPage (Entry{..}) = standardPage
  (title $ toMarkup entryTitle)
  (div ! id "articlepage" $ do
    h1 "Anton Felix Lorenzen"
    nav $ do
      -- TODO: Use the level information to build a nested tree of headlines.
      ul $ forM_ (allHeaders entryContent) $ \(_, nm) -> do
        li $ a ! href (stringValue $ format
                      $ renderMarkup $ contents nm) $ nm)
  (article $ do
    h2 $ toMarkup entryTitle
    writeHtml def (headersMinusTwo entryContent))

  where
    format :: BL.ByteString -> String
    format = ('#':) . fmap (chr . fromIntegral) . BL.unpack . BL.map go
      where
        go :: Word8 -> Word8
        go c = if (chr $ fromIntegral c) == ' '
          then fromIntegral $ ord '-'
          else fromIntegral . ord . toLower . chr $ fromIntegral c
