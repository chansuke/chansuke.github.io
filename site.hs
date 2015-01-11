{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Data.Char (ord)
import Data.Map (lookup)
import Data.Monoid ((<>), mconcat, mempty)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/style.scss" $ do
        route   $ setExtension ".css"
        compile $ getResourceString >>= sassify

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
            pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= saveSnapshot "content" -- ^ For RSS
            >>= loadAndApplyTemplate "templates/post.html"
                       (postCtx tags <> defaultContext)
            >>= finish    (titleCtx "Blog")

    create ["posts.html"] $ do
        route idRoute
        compile $ postPage tags "All Posts" "Posts/*"

    tagsRules tags $ \tag pattern -> do
        let title = "Tag: " ++ tag
        route idRoute
        compile $ postPage tags title pattern

    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 5) . recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html"
                        (constField "posts" list <> defaultContext)
                >>= finish (titleCtx "Home")

    match "pages/*.markdown" $ do
        route   $ composeRoutes (gsubRoute "pages/" (const "")) (setExtension ".html")
        compile $ pandocCompiler >>= finish mempty

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst
                =<< loadAllSnapshots "posts/*" "content"
            renderRSS feedConfiguration (bodyField "description" <>
                defaultContext) posts

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
