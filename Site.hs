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

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "_posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
      >>= loadAndApplyTemplate "_layouts/default.html" postCtx
      >>= relativizeUrls


  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "_posts/*"
      let indexCtx
            =  listField "posts" postCtx (return posts)
            <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
        >>= relativizeUrls

  match "_layouts/*" $ compile templateBodyCompiler
  match "_includes/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx
  =  dateField "date" "%b %-d, %Y"
  <> defaultContext

defaultContext :: Context String
defaultContext
  =  constField "siteTitle" "Antitypical"
  <> constField "baseUrl" ""
  <> constField "siteUrl" "https://antitypical.com"
  <> Hakyll.defaultContext
