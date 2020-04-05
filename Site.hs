{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import           Hakyll hiding (defaultContext)
import qualified Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "icon*" $ do
    route   idRoute
    compile copyFileCompiler

  match "_css/*" $
    compile compressCssCompiler

  create ["css/stylesheet.css"] $ do
    route idRoute
    compile $ do
      csses <- loadAll "_css/*.css"
      makeItem . unlines $ map itemBody csses

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx
            =  listField "posts" postCtx (return posts)
            <> bodyField "body"
            <> antitypicalContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
        >>= relativizeUrls

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

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Antitypical"
  , feedDescription = "Words by Rob Rix."
  , feedAuthorName  = "Rob Rix"
  , feedAuthorEmail = "rob.rix@me.com"
  , feedRoot        = "https://antitypical.com"
  }
