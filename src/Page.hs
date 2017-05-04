{-# Language NoOverloadedLists #-}

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
import qualified Text.Blaze.Renderer.Text as TextRenderer
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
      let poss = fmap (("posts/"<>) . (<>".html") . T.intercalate "-" . fmap displayUrl)
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

standardPage :: Markup -> Markup -> Markup -> Markup -> BL.ByteString
standardPage pageTitle pageDescription sidebarPart mainPart = renderMarkup $ html $ do
  head $ do
    title pageTitle
    meta ! charset "UTF-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
    meta ! name "description" ! (content $ toAttr pageDescription)
    meta ! name "twitter:card" ! content "summary"
    meta ! name "twitter:site" ! content "@anton_lorenzen"
    meta ! name "twitter:title" ! content (toAttr pageTitle)
    meta ! name "twitter:description" ! content (toAttr pageDescription)
    -- meta ! name "twitter:image" ! content ""
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
          sidebarPart
      div ! id "main" $ do
        div ! class_ "header" $ do
          h1 pageTitle
          p pageDescription
        div ! class_ "content" $ do
          mainPart
        div ! class_ "footer" $ do
          div ! id "imprint-link" $ do
            a ! href "/blog/imprint.html" $ "Imprint"
          div ! id "privacy-link" $ do
            a ! href "/blog/privacy.html" $ "Privacy"
    script $ toMarkup ($(loadJavaScript) :: Text)
  where
    toAttr = toValue . TextRenderer.renderMarkup . contents


renderFrontPage :: (Enum a, Bounded a, Display a) => Maybe a -> [Headline] -> BL.ByteString
renderFrontPage ma headlines = standardPage
  (toMarkup $ maybe "Posts" displayTitle ma)
  (toMarkup $ maybe "" displayDescription ma)
  (div ! id "frontpage" $ do
    ul ! class_ "pure-menu-list"
       $ forM_ ([minBound .. maxBound] :: [Category]) $ \c ->
          li ! class_ "pure-menu-item"
             $ a ! class_ "pure-menu-link" ! href (stringValue $ T.unpack $ "/blog/" <> displayUrl c <> "/")
                 $ toMarkup $ displayTitle c)
  (ul $ do
    forM_ headlines $ \(Headline {..}) ->
      li $ do
        h2 $ do
          a ! href (stringValue . ("/blog/"<>) . T.unpack $ headlineURL) $ toMarkup $ headlineTitle
        p $ preEscapedString $ headlineAbstract)


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

renderPage :: Entry Pandoc -> BL.ByteString
renderPage (Entry{..}) = standardPage
  (toMarkup entryTitle)
  (toMarkup $ writeHtml def entryAbstract)
  -- TODO: Use the level information to build a nested tree of headlines.
  (ul ! class_ "pure-menu-list"
      $ forM_ (allHeaders entryContent) $ \(_, nm) -> do
         li ! class_ "pure-menu-item"
            $ a ! class_ "pure-menu-link"
                ! href (stringValue $ format $ renderMarkup $ contents nm)
                $ nm)
  (article $ do
    writeHtml def (headersMinusTwo entryContent))

  where
    format :: BL.ByteString -> String
    format = ('#':) . fmap (chr . fromIntegral) . BL.unpack . BL.map go
      where
        go :: Word8 -> Word8
        go c = if (chr $ fromIntegral c) == ' '
          then fromIntegral $ ord '-'
          else fromIntegral . ord . toLower . chr $ fromIntegral c


renderImprint :: BL.ByteString
renderImprint = standardPage
  "Impressum"
  ""
  mempty
  (article $ do
    h3 "Angaben gemäß § 5 TMG:"
    p "Anton Felix Lorenzen"
    p "Osterbekstr. 102"
    p "22083 Hamburg"
    h3 "Kontakt:"
    p "Telefon: +49 (40) 38 65 39 73"
    p "E-Mail: anfelor@posteo.de"
    h3 "Verantwortlich für den Inhalt nach § 55 Abs. 2 RStV:"
    p "Anton Felix Lorenzen"
    p "Osterbekstr. 102"
    p "22083 Hamburg"
    h3 "Haftung für Inhalte"
    p $ do
      "Als Diensteanbieter sind wir gemäß § 7 Abs.1 TMG für eigene Inhalte auf diesen Seiten nach den"
      "allgemeinen Gesetzen verantwortlich. Nach §§ 8 bis 10 TMG sind wir als Diensteanbieter jedoch nicht"
      "verpflichtet, übermittelte oder gespeicherte fremde Informationen zu überwachen oder nach Umständen"
      "zu forschen, die auf eine rechtswidrige Tätigkeit hinweisen.Verpflichtungen zur Entfernung oder Sperrung der Nutzung von Informationen nach den allgemeinen"
      "Gesetzen bleiben hiervon unberührt. Eine diesbezügliche Haftung ist jedoch erst ab dem Zeitpunkt der"
      "Kenntnis einer konkreten Rechtsverletzung möglich. Bei Bekanntwerden von entsprechenden"
      "Rechtsverletzungen werden wir diese Inhalte umgehend entfernen."
    h3 "Haftung für Links"
    p $ do
      "Unser Angebot enthält Links zu externen Webseiten Dritter, auf deren Inhalte wir keinen Einfluss haben."
      "Deshalb können wir für diese fremden Inhalte auch keine Gewähr übernehmen. Für die Inhalte der"
      "verlinkten Seiten ist stets der jeweilige Anbieter oder Betreiber der Seiten verantwortlich. Die verlinkten"
      "Seiten wurden zum Zeitpunkt der Verlinkung auf mögliche Rechtsverstöße überprüft. Rechtswidrige"
      "Inhalte waren zum Zeitpunkt der Verlinkung nicht erkennbar."
    p $ do
      "Eine permanente inhaltliche Kontrolle der verlinkten Seiten ist jedoch ohne konkrete Anhaltspunkte einer"
      "Rechtsverletzung nicht zumutbar. Bei Bekanntwerden von Rechtsverletzungen werden wir derartige Links"
      "umgehend entfernen."
    h3 "Urheberrecht"
    p $ do
      "Die durch die Seitenbetreiber erstellten Inhalte und Werke auf diesen Seiten unterliegen dem deutschen"
      "Urheberrecht. Die Vervielfältigung, Bearbeitung, Verbreitung und jede Art der Verwertung außerhalb der"
      "Grenzen des Urheberrechtes bedürfen der schriftlichen Zustimmung des jeweiligen Autors bzw."
      "Erstellers. Downloads und Kopien dieser Seite sind nur für den privaten, nicht kommerziellen Gebrauch"
      "gestattet."
    p $ do
      "Soweit die Inhalte auf dieser Seite nicht vom Betreiber erstellt wurden, werden die Urheberrechte Dritter"
      "beachtet. Insbesondere werden Inhalte Dritter als solche gekennzeichnet. Sollten Sie trotzdem auf eine"
      "Urheberrechtsverletzung aufmerksam werden, bitten wir um einen entsprechenden Hinweis. Bei"
      "Bekanntwerden von Rechtsverletzungen werden wir derartige Inhalte umgehend entfernen."
    p $ i $ do
      "Quelle: "
      a ! href "https://www.e-recht24.de" $ "https://www.e-recht24.de")

renderPrivacy :: BL.ByteString
renderPrivacy = standardPage
  "Datenschutz"
  ""
  mempty
  (article $ do
    p "Die Betreiber dieser Seiten nehmen den Schutz Ihrer persönlichen Daten sehr ernst. Wir behandeln Ihre personenbezogenen Daten vertraulich und entsprechend der gesetzlichen Datenschutzvorschriften sowie dieser Datenschutzerklärung."
    p "Die Nutzung unserer Webseite ist in der Regel ohne Angabe personenbezogener Daten möglich. Soweit auf unseren Seiten personenbezogene Daten (beispielsweise Name, Anschrift oder E-Mail-Adressen) erhoben werden, erfolgt dies, soweit möglich, stets auf freiwilliger Basis. Diese Daten werden ohne Ihre ausdrückliche Zustimmung nicht an Dritte weitergegeben."
    p "Wir weisen darauf hin, dass die Datenübertragung im Internet (z.B. bei der Kommunikation per E-Mail) Sicherheitslücken aufweisen kann. Ein lückenloser Schutz der Daten vor dem Zugriff durch Dritte ist nicht möglich."
    h3 "Cookies"
    p "Die Internetseiten verwenden teilweise so genannte Cookies. Cookies richten auf Ihrem Rechner keinen Schaden an und enthalten keine Viren. Cookies dienen dazu, unser Angebot nutzerfreundlicher, effektiver und sicherer zu machen. Cookies sind kleine Textdateien, die auf Ihrem Rechner abgelegt werden und die Ihr Browser speichert."
    p "Die meisten der von uns verwendeten Cookies sind so genannte „Session-Cookies“. Sie werden nach Ende Ihres Besuchs automatisch gelöscht. Andere Cookies bleiben auf Ihrem Endgerät gespeichert, bis Sie diese löschen. Diese Cookies ermöglichen es uns, Ihren Browser beim nächsten Besuch wiederzuerkennen."
    p "Sie können Ihren Browser so einstellen, dass Sie über das Setzen von Cookies informiert werden und Cookies nur im Einzelfall erlauben, die Annahme von Cookies für bestimmte Fälle oder generell ausschließen sowie das automatische Löschen der Cookies beim Schließen des Browser aktivieren. Bei der Deaktivierung von Cookies kann die Funktionalität dieser Website eingeschränkt sein."
    h3 "Datenschutzerklärung für die Nutzung von Google Analytics"
    p "Diese Website nutzt Funktionen des Webanalysedienstes Google Analytics. Anbieter ist die Google Inc., 1600 Amphitheatre Parkway Mountain View, CA 94043, USA."
    p "Google Analytics verwendet so genannte \"Cookies\". Das sind Textdateien, die auf Ihrem Computer gespeichert werden und die eine Analyse der Benutzung der Website durch Sie ermöglichen. Die durch den Cookie erzeugten Informationen über Ihre Benutzung dieser Website werden in der Regel an einen Server von Google in den USA übertragen und dort gespeichert."
    p $ do
      "Mehr Informationen zum Umgang mit Nutzerdaten bei Google Analytics finden Sie in der Datenschutzerklärung von Google: "
      a ! href "https://support.google.com/analytics/answer/6004245?hl=de" $ "https://support.google.com/analytics/answer/6004245?hl=de"
    h4 "Browser Plugin"
    p $ do
      "Sie können die Speicherung der Cookies durch eine entsprechende Einstellung Ihrer Browser-Software verhindern; wir weisen Sie jedoch darauf hin, dass Sie in diesem Fall gegebenenfalls nicht sämtliche Funktionen dieser Website vollumfänglich werden nutzen können. Sie können darüber hinaus die Erfassung der durch den Cookie erzeugten und auf Ihre Nutzung der Website bezogenen Daten (inkl. Ihrer IP-Adresse) an Google sowie die Verarbeitung dieser Daten durch Google verhindern, indem Sie das unter dem folgenden Link verfügbare Browser-Plugin herunterladen und installieren: "
      a ! href "https://tools.google.com/dlpage/gaoptout?hl=de" $ "https://tools.google.com/dlpage/gaoptout?hl=de"
    h4 "Widerspruch gegen Datenerfassung"
    p $ do
      "Sie können die Erfassung Ihrer Daten durch Google Analytics verhindern, indem Sie auf folgenden Link klicken. Es wird ein Opt-Out-Cookie gesetzt, der die Erfassung Ihrer Daten bei zukünftigen Besuchen dieser Website verhindert: "
      a ! href "javascript:gaOptout()" $ "Google Analytics deaktivieren"
    h4 "Auftragsdatenverarbeitung"
    "Wir haben mit Google einen Vertrag zur Auftragsdatenverarbeitung abgeschlossen und setzen die strengen Vorgaben der deutschen Datenschutzbehörden bei der Nutzung von Google Analytics vollständig um."
    h4 "IP-Anonymisierung"
    p "Wir nutzen die Funktion \"Aktivierung der IP-Anonymisierung\" auf dieser Webseite. Dadurch wird Ihre IP-Adresse von Google jedoch innerhalb von Mitgliedstaaten der Europäischen Union oder in anderen Vertragsstaaten des Abkommens über den Europäischen Wirtschaftsraum zuvor gekürzt. Nur in Ausnahmefällen wird die volle IP-Adresse an einen Server von Google in den USA übertragen und dort gekürzt. Im Auftrag des Betreibers dieser Website wird Google diese Informationen benutzen, um Ihre Nutzung der Website auszuwerten, um Reports über die Websiteaktivitäten zusammenzustellen und um weitere mit der Websitenutzung und der Internetnutzung verbundene Dienstleistungen gegenüber dem Websitebetreiber zu erbringen. Die im Rahmen von Google Analytics von Ihrem Browser übermittelte IP-Adresse wird nicht mit anderen Daten von Google zusammengeführt."
    h3 "SSL-Verschlüsselung"
    p "Diese Seite nutzt aus Gründen der Sicherheit und zum Schutz der Übertragung vertraulicher Inhalte, wie zum Beispiel der Anfragen, die Sie an uns als Seitenbetreiber senden, eine SSL-Verschlüsselung. Eine verschlüsselte Verbindung erkennen Sie daran, dass die Adresszeile des Browsers von \"http://\" auf \"https://\" wechselt und an dem Schloss-Symbol in Ihrer Browserzeile."
    p "Wenn die SSL Verschlüsselung aktiviert ist, können die Daten, die Sie an uns übermitteln, nicht von Dritten mitgelesen werden."
    p $ i $ do
      "Quellverweis: "
      a ! href "https://www.e-recht24.de" $ "https://www.e-recht24.de")
