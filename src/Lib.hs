{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SVGA
import qualified Text.Blaze.Internal as I
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Shapes
import Data.Text.Lazy (Text,unpack)
import Text.Blaze.Html5 (toHtml)

beamMeUpScotty = scotty 3000 $ do
  get "/" $ html "Hello World!"

  get "/shapes" $ html testForm

  post "/shapes/create" $ do
      style <- param "Styles"
      trans <- param "Transforms"
      shape <- param "Shapes"
      html $ buildShape style trans shape

  get "/shapes/create" $ do
        style <- param "Styles"
        trans <- param "Transforms"
        shape <- param "Shapes"
        html $ buildShape style trans shape

  get "/svg/rotated" $ html $ rotatedSquare

sillyPrint = print "boo"

buildShape :: Text -> Text -> Text ->Text
buildShape style trans shape = R.renderHtml $
  do H.head $ H.title "Shape SVG"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg $ customSvg (unpack style) (unpack trans) (unpack shape)

--test :: Text
--test = R.renderHtml $
--  do H.head $ H.title "Test"
--     H.body $
--      H.form $ H.preEscapedToHtml $ renderSvg svgColour

testForm :: Text
testForm = R.renderHtml $
  do H.head $ H.title "Form"
     H.body $
      H.form ! A.action "/shapes/create" $ do
        H.input ! A.type_ "text" ! A.name "Styles" ! A.value "FillColour 0.5 0.1 0"
        H.input ! A.type_ "text" ! A.name "Transforms" ! A.value "Rotate 45"
        H.input ! A.type_ "text" ! A.name "Shapes" ! A.value "Square"
        H.input ! A.type_ "submit" ! A.value "Submit"

rotatedSquare :: Text
rotatedSquare = R.renderHtml $
   do H.head $ H.title "Form"
      H.body $
        H.div $ H.preEscapedToHtml $ renderSvg $ rotatedSvg

---------------------------------------------------------------------------------------------------------------------------------------
customSvg :: String -> String -> String -> S.Svg
customSvg style trans shape = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "100%" ! SVGA.height "100%" ! SVGA.viewbox "-25 -5 50 50" $ S.g $ buildCustomSvgFromString style trans shape

rotatedSvg :: S.Svg
rotatedSvg = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "100%" ! SVGA.height "100%" ! SVGA.viewbox "-25 -5 50 50" $ S.g $ roatedSquare


makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5
