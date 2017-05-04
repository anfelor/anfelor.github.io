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
import System.Process as Exports

import Text.Pandoc as Exports hiding (Space, Format, Reader, Meta)

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Prelude
import GHC.IO.Handle
import Data.List


class Show a => Display a where
  displayTitle :: a -> Text
  displayTitle a = "Posts about " <> displayShow a <> "."

  displayDescription :: a -> Text
  displayDescription a = "Posts about " <> displayShow a <> "."

  -- | Turn the showable into a string, splitting it into it's word parts
  -- and joining them with a dash '-'. The result is all lower case.
  displayUrl :: a -> Text
  displayUrl a = case T.uncons (displayShow a) of
    Just (h, t) -> T.foldl' go (T.singleton (toLower h)) $ T.concatMap escape t
    Nothing -> undefined
    where
      go :: Text -> Char -> Text
      go t c = case () of
        _ | isUpper c && isLower (T.last t) -> t <> "-" <> T.singleton (toLower c)
          | c == ' '  -> case T.last t of
              '-' -> t
              _ -> T.snoc t '-'
          | otherwise -> T.snoc t c

      escape c = case c of
        'ä' -> "ae"
        'ö' -> "oe"
        'ü' -> "ue"
        'Ä' -> "Ae"
        'Ö' -> "Oe"
        'Ü' -> "Ue"
        _ | isAlphaNum c -> T.singleton c
        _ -> ""

  -- | Allows you to keep the default implementation of the
  -- functions above, while customizing the way your datatype is shown.
  -- It must guarantee to return non-empty text.
  displayShow :: a -> Text
  displayShow = show

instance Display T.Text where
  displayShow = identity


md :: QuasiQuoter
md = QuasiQuoter
  { quoteExp = \e -> [| readMarkdown def $(extractVariables e)|]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

rst :: QuasiQuoter
rst = QuasiQuoter
  { quoteExp = \e -> [| readRST def $(extractVariables e)|]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

latex :: QuasiQuoter
latex = QuasiQuoter
  { quoteExp = \e -> [| readLaTeX def $(extractVariables e)|]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

txt :: QuasiQuoter
txt = QuasiQuoter
  { quoteExp = extractVariables
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

-- | Extract the variables from a string using Shakespeare-like escapes:
-- `[txt|Hello #{user}!|]`. Other constructs are not provided.
-- Supports variable interpolation and non-nested prefix application.
extractVariables :: String -> ExpQ
extractVariables s = go2 . snd $ go s [""]
  where
    go2 (x:y:xs) = [| $(litE $ stringL x) <> $(toVars $ trim y) <> $(go2 xs) |]
    go2 (x:[]) = [| $(litE $ stringL x) |]
    go2 [] = [| "" |]

    trim = dropWhile (==' ') . dropWhileEnd (==' ')

    toVars xs =
      let (varName, rest) = dropWhile (==' ') <$> break (==' ') xs
      in case rest of
        [] -> dyn varName
        str -> toVars' (appE (dyn varName)) str

    toVars' apps xs =
      let (varName, rest) = dropWhile (==' ') <$> break (==' ') xs
      in case rest of
        [] -> apps (dyn varName)
        str -> toVars' (appE (apps (dyn varName))) str

    go :: String -> [String] -> (String, [String])
    go ('#':'{':xs) parts =
      let (varName, _:rest) = break (=='}') xs
          ("", p) = go rest parts
      in ("", "":varName:p)
    go ( x :xs) parts = (toHead ( x :)) <$> go xs parts
    go [] parts = ("", parts)

    toHead _ [] = undefined
    toHead f (x:xs) = f x : xs


-- | Read a file at compile time, "returns" a string literal subject to OverloadedStrings.
readFileCompileTime :: FilePath -> Q Exp
readFileCompileTime = fmap (LitE . StringL) . runIO . Prelude.readFile

-- | Load the javascript in js/main.js at compile time
-- and return them as a string literal.
-- This compiles and uglifies the typescript data.
-- DANGER: This assumes uses relative paths and can only be run from the project root!
loadJavaScript :: Q Exp
loadJavaScript = fmap (LitE . StringL) . runIO $ do
  _ <- createProcess (proc "tsc" ["-p", "js/"])
  (_, Just hout, _, _) <- createProcess (proc "uglifyjs" ["js/main.js"])
                                        { std_out = CreatePipe }
  hGetContents hout


-- | Load the css in css/main.css at compile time
-- and return them as a string literal.
-- DANGER: This assumes uses relative paths and can only be run from the project root!
loadCss :: Q Exp
loadCss = fmap (LitE . StringL) . runIO $ do
  (_, Just hout, _, _) <- createProcess (shell "sass css/main.scss | cssnano")
                                        { std_out = CreatePipe }
  hGetContents hout
