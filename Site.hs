{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import           Data.List
import           Hakyll hiding (defaultContext)
import qualified Hakyll
import           System.FilePath
import           Text.Pandoc.Options

main :: IO ()
main = hakyllWith defaultConfiguration{ destinationDirectory = "docs" } $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "icon*" $ do
    route   idRoute
    compile copyFileCompiler

  match "_css/*" $
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  create ["css/stylesheet.css"] $ do
    route idRoute
    compile $ do
      csses <- loadAll "_css/*.css"
      makeItem . unlines $ map itemBody csses

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("antitypical.com" :: String)

  match "posts/*.md" $ do
    route cleanRoute
    compile $ pandocCompilerWith readerOptions defaultHakyllWriterOptions
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

  match "posts/*.html" $ do
    route cleanRoute
    compile $ getResourceBody >>= applyAsTemplate postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx
            =  listField "posts" postCtx (return posts)
            <> bodyField "body"
            <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let feedCtx
            =  postCtx
            <> bodyField "description"

      renderAtom feedConfig feedCtx posts

  match "_layouts/*" $ compile templateBodyCompiler
  match "_includes/*" $ compile templateBodyCompiler

  match "404.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "_layouts/default.html" defaultContext
      >>= relativizeUrls
      >>= cleanIndexUrls

  version "redirects" $ createRedirects
    [ ("swift/2015/07/01/pattern-matching-over-recursive-values-in-swift/index.html", "/posts/2015-07-01-pattern-matching-over-recursive-values-in-swift/")
    , ("code/2014/04/20/on-the-order-of-neptune/index.html", "/posts/2014-04-20-on-the-order-of-neptune/")
    ]

readerOptions :: ReaderOptions
readerOptions = let opts = defaultHakyllReaderOptions in opts{ readerExtensions = disableExtension Ext_markdown_in_html_blocks (enableExtension Ext_raw_html (readerExtensions opts)) }

postCtx :: Context String
postCtx
  =  dateField "date" "%b %-d, %Y"
  <> teaserField "excerpt" "content"
  <> field "minutes" (pure . show . max 1 . (`div` 180) . length . words . itemBody)
  <> defaultContext

antitypicalContext :: Context String
antitypicalContext
  =  constField "siteTitle" "Antitypical"
  <> constField "siteAuthor" "Rob Rix"
  <> constField "siteDescription" "Words by Rob Rix."
  <> constField "baseUrl" ""
  <> constField "siteUrl" "https://antitypical.com"

defaultContext :: Context String
defaultContext
  =  antitypicalContext
  <> Hakyll.defaultContext
  <> constField "measure" "measure"

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Antitypical"
  , feedDescription = "Words by Rob Rix."
  , feedAuthorName  = "Rob Rix"
  , feedAuthorEmail = "rob.rix@me.com"
  , feedRoot        = "https://antitypical.com"
  }

cleanRoute :: Routes
cleanRoute = customRoute (createIndexRoute . toFilePath)
  where
  createIndexRoute p = takeDirectory p </> takeBaseName p </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = pure . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = pure . fmap (replaceAll pattern replacement)
  where
  pattern = "/index.html"
  replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise            = url
  where
  idx = "index.html"
