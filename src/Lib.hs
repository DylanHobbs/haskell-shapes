{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SVGA
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Shapes
import Ansi
import Data.Text.Lazy
import Text.Blaze.Html5 (toHtml)

beamMeUpScotty = scotty 3000 $ do
  get "/" $ html "Hello World!"

  get "/greet" $ html "Yo"

  get (literal "/greet/") $ html "Oh, wow!"

  get "/svg/circle/:colour" $ do
      colour <- param "colour"
      html $ makeCircle colour


sillyPrint = print "boo"

makeCircle :: Text -> Text
makeCircle colour = R.renderHtml $
  do H.head $ H.title $ "Circle"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg svgDoc


svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "150" ! SVGA.height "100" ! SVGA.viewbox "0 0 3 2" $ S.g $
  do S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#008d46"
     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#ffffff"
     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#d2232c"
     S.path ! SVGA.d makePath

svgColour :: Colour -> S.Svg
svgColour colour = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "150" ! SVGA.height "100" ! SVGA.viewbox "0 0 3 2" $ S.g $
  do S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#008d46"
     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#ffffff"
     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#d2232c"
     S.path ! SVGA.d makePath



makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5
