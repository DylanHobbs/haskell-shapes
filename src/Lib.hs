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

  get "/svg/circle/:r/:g/:b" $ do
      let shape = "Circle"
      r <- param "r"
      g <- param "g"
      b <- param "b"
      html $ makeCircle shape r g b

  get "/svg/square/:r/:g/:b" $ do
      let shape = "Square"
      r <- param "r"
      g <- param "g"
      b <- param "b"
      html $ makeSquare shape r g b

  get "/svg/lines/:d" $ do
      let shape = "Square"
      d <- param "d"
      html $ makeThickCircle d

  get "/svg/rotated" $ html $ rotatedSquare

sillyPrint = print "boo"

buildShape :: Text -> Text -> Text ->Text
buildShape style trans shape = R.renderHtml $
  do H.head $ H.title "Shape SVG"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg $ buildCustomSvgFromString (unpack style) (unpack trans) (unpack shape)

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
        H.input ! A.type_ "text" ! A.name "Styles" ! A.value "Styles"
        H.input ! A.type_ "text" ! A.name "Transforms" ! A.value "Transforms"
        H.input ! A.type_ "text" ! A.name "Shapes" ! A.value "Shapes"
        H.input ! A.type_ "submit" ! A.value "Submit"

rotatedSquare :: Text
rotatedSquare = R.renderHtml $
   do H.head $ H.title "Form"
      H.body $
        H.div $ H.preEscapedToHtml $ renderSvg $ rotatedSvg

makeSquare :: Text -> Text -> Text -> Text -> Text
makeSquare shape r g b = R.renderHtml $
  do H.head $ H.title "Square"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg $ squareSvg  (unpack shape) (read (unpack r)) (read (unpack g)) (read(unpack b))

makeCircle :: Text -> Text -> Text -> Text -> Text
makeCircle shape r g b = R.renderHtml $
  do H.head $ H.title "Circle"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg $ circleSvg  (unpack shape) (read (unpack r)) (read (unpack g)) (read(unpack b))

makeThickCircle :: Text -> Text
makeThickCircle d = R.renderHtml $
  do H.head $ H.title "Thick Circle"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg $ thickCircleSvg (read (unpack d))

---------------------------------------------------------------------------------------------------------------------------------------
customSvg :: String -> String -> String -> S.Svg
customSvg style trans shape = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "100%" ! SVGA.height "100%" ! SVGA.viewbox "-25 -5 50 50" $ S.g $ roatedSquare

rotatedSvg :: S.Svg
rotatedSvg = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "100%" ! SVGA.height "100%" ! SVGA.viewbox "-25 -5 50 50" $ S.g $ roatedSquare

squareSvg :: String -> Double -> Double -> Double -> S.Svg
squareSvg shape r g b = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ colouredSquare shape r g b

circleSvg :: String -> Double -> Double -> Double -> S.Svg
circleSvg shape r g b = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ colouredCircle shape r g b

thickCircleSvg :: Double -> S.Svg
thickCircleSvg d = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ blockyCircle d
--
--svgColour :: S.Svg
--svgColour = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g
--        $ do
--        S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#008d46" ! SVGA.transform (S.rotate 10) (S.scale 3 3)
--        S.path ! SVGA.d makePath


makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5
