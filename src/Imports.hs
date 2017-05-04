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

import qualified Data.Text as T
import Language.Haskell.TH
import qualified Prelude
import GHC.IO.Handle


class Show a => Display a where
  displayTitle :: a -> Text
  displayTitle a = "Posts about " <> show a <> "."

  displayDescription :: a -> Text
  displayDescription a = "Posts about " <> show a <> "."

  -- | Turn the showable into a string, splitting it into it's word parts
  -- and joining them with a dash '-'. The result is all lower case.
  displayUrl :: a -> Text
  displayUrl a = case T.uncons (show a) of
    Just (h, t) -> T.foldl' go (T.singleton (toLower h)) t
    Nothing -> undefined -- show is guaranteed to return non-empty text.
    where
      go :: Text -> Char -> Text
      go txt c = if isUpper c
        then txt <> "-" <> T.singleton (toLower c)
        else T.snoc txt c


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
