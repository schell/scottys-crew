{-# LANGUAGE OverloadedStrings #-}
module Web.Crew.Templates.Renderer (
  blaze,
  blazePretty
) where

import           Web.Scotty
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- | Render some Blaze Html
--
blaze :: Html -> ActionM ()
blaze h = do
  setHeader "Content-Type" "text/html"
  raw $ Utf8.renderHtml h

blazePretty :: Html -> ActionM ()
blazePretty h = do
    setHeader "Content-Type" "text/html"
    raw $ B.pack $ Pretty.renderHtml h
