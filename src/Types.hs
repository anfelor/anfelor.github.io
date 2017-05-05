module Types where

import Imports
import qualified Data.HashSet as Set
import qualified Data.Text as T


-- | A blog entry.
data Entry a = Entry
  { entryTitle :: Text
  , entryCreated :: Day
  , entryUpdated :: Day
  , entryKeywords :: NonEmpty Keyword
  , entryLanguage :: Language
  , entryAbstract :: a
  , entryContent :: a
  , entryComments :: Comments
  } deriving (Show, Functor)

instance Eq (Entry a) where
  a == b = entryTitle a == entryTitle b
        && entryCreated a == entryCreated b

instance Ord (Entry a) where
  compare a b = case comparing entryCreated a b of
    LT -> LT
    GT -> GT
    EQ -> comparing entryTitle a b


-- | A reduced version of a blog post,
-- featuring only information used in the
-- overview pages.
data Headline = Headline
  { headlineTitle :: Text
  , headlineCreated :: Day
  , headlineUpdated :: Day
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
    go e@Entry{..} = do
      used <- get
      let poss = fmap (T.intercalate "-" . fmap displayUrl) $ permutations $ toList entryKeywords
          title = displayUrl entryTitle
          urls = fmap (\p -> [txt|posts/#{title}-#{p}.html|]) poss
          choose = filter (`notElem` used) urls
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
      , headlineKeywords = entryKeywords
      , headlineAbstract = writeHtmlString def entryAbstract
      , headlineURL = url
      }


-- | The language of a blog post.
data Language
  = English
  | German
  deriving (Eq, Bounded, Enum, Show)


-- | The place where comments should be posted.
data Comments
 = Reddit Text
 | Github -- ^ open a new issue on github.
 deriving (Eq, Show)


-- | The keywords for a blog post.
-- Using Data types instead of text has the advantage,
-- that posts can be grouped by keyword and also, that
-- the number of keywords is limited to 100-1000.
data Keyword
 = Blogging
 | Haskell
 | ReadingList
 | ShortStories
 deriving (Eq, Bounded, Enum, Show)

instance Display Keyword where
  displayTitle Haskell = "Haskell"
  displayTitle ReadingList = "My reading list"
  displayTitle ShortStories = "Short stories"
  displayTitle Blogging = "General information"

  displayDescription Haskell = "Haskell is a functional programming language; I try to explain it's concepts."
  displayDescription ReadingList = "I set myself a goal of reading a book a week and write a small entry about each."
  displayDescription ShortStories = "I am writing short stories about places, people and things, that left an impression."
  displayDescription Blogging = "This is a catch-all category for the maintenance of this blog."
