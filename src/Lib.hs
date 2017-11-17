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
import Data.Text.Lazy (Text,unpack)
import Text.Blaze.Html5 (toHtml)

beamMeUpScotty = scotty 3000 $ do
  get "/" $ html "Hello World!"

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

sillyPrint = print "boo"

testForm :: Text
testForm = R.renderHtml $
  do H.head $ H.title "Form"
     H.body $
      H.form $ H.span "foo"

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


--svgDoc :: S.Svg
--svgDoc = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "150" ! SVGA.height "100" $ S.g $
--  do S.circle ! SVGA.cx "50" ! SVGA.cy "50" ! SVGA.r "50" ! SVGA.fill "#ff0000"

--svgTest :: S.Svg
--svgTest = svgHead
--   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "0 0 3 2" $ S.g $runTest

squareSvg :: String -> Double -> Double -> Double -> S.Svg
squareSvg shape r g b = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ colouredSquare shape r g b

circleSvg :: String -> Double -> Double -> Double -> S.Svg
circleSvg shape r g b = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ colouredCircle shape r g b

thickCircleSvg :: Double -> S.Svg
thickCircleSvg d = svgHead
   where svgHead = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "500" ! SVGA.height "500" ! SVGA.viewbox "-25 -25 50 50" $ S.g $ blockyCircle d

--svgColour :: Colour -> S.Svg
--svgColour colour = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "150" ! SVGA.height "100" ! SVGA.viewbox "0 0 3 2" $ S.g $
--  do S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#008d46"
--     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#ffffff"
--     S.rect ! SVGA.width "1" ! SVGA.height "2" ! SVGA.fill "#d2232c"
--     S.path ! SVGA.d makePath

makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5
