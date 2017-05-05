{-# Language NoOverloadedLists #-}

module Page where

import Imports hiding (head, link, div, map)
import Types

import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy as BL
import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Text.Blaze.Renderer.Text as TextRenderer
import qualified Data.Text as T
import Data.Time.Format


data Page = Page
  { pageTitle :: Markup
  , pageDescription :: Markup
  , pageUrl :: Text
  , pageInfo :: Maybe PageInfo
  , sidebarTop :: Markup
  , sidebarBottom :: Markup
  , mainContent :: Markup
  }

data PageInfo = PageInfo
  { pageKeywords :: NonEmpty Keyword
  , pageCreated :: Day
  , pageUpdated :: Day
  }

standardPage :: Page -> BL.ByteString
standardPage Page{..} = renderMarkup $ do
  docType
  html ! lang "en_US" $ do
    head $ do
      let url = "https://anfelor.github.io/blog/" <> pageUrl

      title pageTitle
      meta ! charset "UTF-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
      meta ! name "description" ! content (toAttr pageDescription)
      meta ! name "author" ! content "Anton Felix Lorenzen"

      link ! rel "canonical" ! href (toValue url)

      link ! rel "shortcut icon" ! href "/img/favicon.ico"
      link ! rel "icon" ! type_ "image/png" ! href "/img/favicon.png" ! sizes "32x32"
      link ! rel "apple-touch-icon" ! sizes "180x180" ! href "/img/apple-touch-icon.png"
      meta ! name "msapplication-TileColor" ! content "#ffffff"
      meta ! name "msapplication-TileImage" ! content "/img/mstile-144x144.png"

      -- Open graph protocol. See http://ogp.me/ for more information.
      meta ! customAttribute "property" "og:title" ! content (toAttr pageTitle)
      meta ! customAttribute "property" "og:description" ! content (toAttr pageDescription)
      meta ! customAttribute "property" "og:locale" ! content "en_US" -- TODO: Add i18n
      meta ! customAttribute "property" "og:site_name" ! content "Anton Lorenzen's blog"
      meta ! customAttribute "property" "og:url" ! content (toValue url)
      case pageInfo of
        Nothing -> meta ! customAttribute "property" "og:type" ! content "website"
        Just PageInfo{..} -> do
          meta ! customAttribute "property" "og:type" ! content "article"
          meta ! customAttribute "property" "og:article:published_time"
              ! content (toValue $ formatTime defaultTimeLocale "%F" pageCreated)
          meta ! customAttribute "property" "og:article:modified_time"
              ! content (toValue $ formatTime defaultTimeLocale "%F" pageUpdated)
          meta ! customAttribute "property" "og:article:author" ! content "Anton Felix Lorenzen"
          forM_ pageKeywords $ \kw -> do
            meta ! customAttribute "property" "og:article:tag" ! content (toValue $ displayTitle kw)
      meta ! customAttribute "property" "og:image" ! content "https://anfelor.github.io/img/anfelor_profile.jpg"

      -- Twitter cards
      meta ! name "twitter:card" ! content "summary"
      meta ! name "twitter:site" ! content "@anton_lorenzen"
      meta ! name "twitter:title" ! content (toAttr pageTitle)
      meta ! name "twitter:description" ! content (toAttr pageDescription)
      meta ! name "twitter:image" ! content "https://anfelor.github.io/img/anfelor_profile.jpg"

      link ! rel "stylesheet" ! href "https://unpkg.com/purecss@0.6.2/build/pure-min.css"
          ! customAttribute "integrity" "sha384-UQiGfs9ICog+LwheBSRCt1o5cbyKIHbwjWscjemyBMT9YCUMZffs6UqUTd0hObXD"
          ! customAttribute "crossorigin" "anonymous"
      link ! rel "stylesheet" ! href "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"
      Text.Blaze.Html5.style $ toMarkup ($(loadCss) :: Text)
    body $ do
      div ! id "layout" $ do
        -- Hamburger menu
        a ! href "#menu" ! id "menuLink" ! class_ "menu-link"
          $ Text.Blaze.Html5.span $ mempty
        div ! id "menu" $ do
          div ! class_ "pure-menu" $ do
            a ! class_ "pure-menu-heading" ! href "/blog" $ "Anton Felix Lorenzen"
            sidebarTop
          div ! class_ "pure-menu menu-bottom" $ do
            sidebarBottom
        div ! id "main" $ do
          div ! class_ "header" $ do
            h1 pageTitle
            p pageDescription
          div ! class_ "content" $ do
            mainContent
          div ! class_ "footer" $ do
            div ! id "imprint-link" $ do
              a ! href "/blog/imprint.html" $ "Imprint"
            div ! id "privacy-link" $ do
              a ! href "/blog/privacy.html" $ "Privacy"
      script $ toMarkup ($(loadJavaScript) :: Text)
  where
    toAttr = toValue . TextRenderer.renderMarkup . contents


renderFrontPage :: Maybe Keyword -> [Headline] -> BL.ByteString
renderFrontPage ma headlines = standardPage $ Page
  { pageTitle = toMarkup $ maybe "Posts" displayTitle ma
  , pageDescription = toMarkup $ maybe "" displayDescription ma
  , pageUrl = url
  , pageInfo = Nothing
  , sidebarTop = ul ! class_ "pure-menu-list"
        $ forM_ ([minBound .. maxBound] :: [Keyword]) $ \c ->
            li ! class_ "pure-menu-item"
              $ a ! class_ "pure-menu-link" ! href (stringValue $ T.unpack $ "/blog/" <> displayUrl c <> "/")
                  $ toMarkup $ displayTitle c
  , sidebarBottom = ul ! class_ "pure-menu-list" $ do
      li ! class_ "pure-menu-item" $ a ! class_ "pure-menu-link" ! href "https://twitter.com/anton_lorenzen" $ "Twitter"
      li ! class_ "pure-menu-item" $ a ! class_ "pure-menu-link" ! href "https://github.com/anfelor" $ "Github"
  , mainContent = ul $ do
      forM_ headlines $ \(Headline {..}) ->
        li $ do
          h2 $ do
            a ! href (stringValue . ("/blog/"<>) . T.unpack $ headlineURL) $ toMarkup $ headlineTitle
          p $ preEscapedString $ headlineAbstract
  }
  where
    url = maybe "" displayUrl ma


headersMinusTwo :: Pandoc -> Pandoc
headersMinusTwo = walk go
  where
    go :: Block -> Block
    go (Header n attr inl) = Header (n+1) attr inl
    go block = block

allHeaders :: Pandoc -> [(Int, Html)]
allHeaders pan@(Pandoc mt _)= query go pan
  where
    go :: Block -> [(Int, Html)]
    go (Header n _ inl) = [(n, writeHtml def (Pandoc mt [Plain inl]))]
    go _ = []

renderPage :: (Text, Entry Pandoc) -> BL.ByteString
renderPage (url, Entry{..}) = standardPage $ Page
  { pageTitle = toMarkup entryTitle
  , pageDescription = toMarkup $ writeHtml def entryAbstract
  , pageUrl = url
  , pageInfo = Just $ PageInfo
    { pageKeywords = entryKeywords
    , pageCreated = entryCreated
    , pageUpdated = entryUpdated
    }
  -- TODO: Use the level information to build a nested tree of headlines.
  , sidebarTop = ul ! class_ "pure-menu-list"
      $ forM_ (allHeaders entryContent) $ \(_, nm) -> do
         li ! class_ "pure-menu-item"
            $ a ! class_ "pure-menu-link"
                ! href (stringValue $ format $ renderMarkup $ contents nm)
                $ nm
  , sidebarBottom = ul ! class_ "pure-menu-list" $ do
      li ! class_ "pure-menu-item" $ a ! class_ "pure-menu-link"
                                       ! href commentUrl $ commentText
      li ! class_ "pure-menu-item" $ a ! class_ "pure-menu-link"
                                       ! href (toValue $ ("/blog/"<>) $ T.unpack url -<.> "pdf")
                                       $ "Download as PDF"
  , mainContent = article $ do
      writeHtml def (headersMinusTwo entryContent)
  }
  where
    (commentUrl, commentText) = case entryComments of
      Reddit t -> (toValue t, "Comment on Reddit")
      Github -> ("https://github.com/anfelor/anfelor.github.io/issues/new", "Comment on Github")

    format :: BL.ByteString -> String
    format = ('#':) . fmap (chr . fromIntegral) . BL.unpack . BL.map go
      where
        go :: Word8 -> Word8
        go c = if (chr $ fromIntegral c) == ' '
          then fromIntegral $ ord '-'
          else fromIntegral . ord . toLower . chr $ fromIntegral c
