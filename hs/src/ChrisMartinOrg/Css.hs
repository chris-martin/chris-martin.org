{-# LANGUAGE OverloadedStrings #-}

module ChrisMartinOrg.Css
  ( styleLink
  , compileCss
  , compileCssSource
  , compileCssFallback
  ) where

import ChrisMartinOrg.Core
import ChrisMartinOrg.Hash (writeHashBS)

import Relude
import Prelude ()

import Data.Semigroup ((<>))
import Data.String (fromString)
import Text.Blaze.Html5 as H hiding (main)

import System.Exit
import System.Process.Typed

import qualified System.Directory as Dir
import qualified Text.Blaze.Html5.Attributes as A

compileCss :: Css -> IO (Either String CompiledCss)
compileCss (CssSource path) = compileCssSource path
compileCss (CssCompiled path) = pure $ Right path

compileCssFallback :: [Css] -> IO (Maybe CompiledCss)
compileCssFallback [] = pure Nothing
compileCssFallback (x:xs) =
  do
    e <- compileCss x
    either fail' (return . Just) e
  where
    fail' err = do
      putStrLn err
      compileCssFallback xs

compileCssSource :: FilePath -> IO (Either String CompiledCss)
compileCssSource inFile =
  do
    exists <- Dir.doesFileExist inFile
    if exists
      then do
        (exitCode, stdout, stderr) <- readProcess $ proc "sassc" ["--style", "compact", inFile]
        case exitCode of
          ExitFailure{} -> return $ Left $ toString (decodeUtf8 stderr :: LText)
          ExitSuccess -> Right <$> CompiledCss <$> writeHashBS (toStrict stdout) "css"
      else do
        return $ Left ("Missing CSS: " <> inFile)

styleLink :: CompiledCss -> Html
styleLink href =
  link ! A.rel "stylesheet"
       ! A.type_ "text/css"
       ! A.href (fromString $ compiledCssPath href)
