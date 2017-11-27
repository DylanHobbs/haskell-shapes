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

{-|
  Scotty entrypoint for the WebServer.
-}
beamMeUpScotty = scotty 3000 $ do
  get "/" $ html "Go to /shapes for the form"

  {-|
    Just displays the form. Nothing fancy
  -}
  get "/shapes" $ html testForm

  {-|
    Shape creation page.
    Grabs the user input
  -}
  get "/shapes/create" $ do
        drawing <- param "FD"
        html $ buildShape drawing


{-|
  Blaze HTML function to create the shape.
  Once the renderSvg call translates this to MarkUpM
  preEscapedHtml can translate it to a form Blaze understands.
  Care has been taken to ensure everything is in the correct html form.
-}
buildShape :: Text -> Text
buildShape draw = R.renderHtml $
  do H.head $ do
      H.title "Shape SVG"
      H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
     H.body $
      H.div $ H.preEscapedToHtml $ renderSvg  $ customSvg' (unpack draw)


{-|
  Blaze form, simply takes user input from a form. Post requests were behaving strangly when parsing
  input. This just gives a get request.
-}
testForm :: Text
testForm = R.renderHtml $
  do H.head $ H.title "Form"
     H.body $
      H.form ! A.action "/shapes/create" $ do
        H.label "Enter a shape"
        H.br
        H.input ! A.style "width:70%"  ! A.type_ "text" ! A.name "FD" ! A.value "[(FillColour 1 0 0 :<++> (StrokeColour 0 0 1 :<++> StrokeWidth 0.6), Translate 1 5, Circle 4), (FillColour 1 0 0 :<++> (StrokeColour 0 0 1 :<++> StrokeWidth 0.6), Translate 12 5, Circle 4), (FillColour 0 1 0 :<++> (StrokeColour 0 0 0 :<++> StrokeWidth 0.6), Translate 5.5 12 :<+> Rotate 45, Rectangle 2 2), (FillColour 0 0 1 :<++> (StrokeWidth 1 :<++> StrokeColour 0 0 0), Translate 6 20, Circle 3), (FillColour 0 0 0 :<++> (StrokeColour 0 0 1 :<++> StrokeWidth 0.6), Translate 1 5, Circle 1), (FillColour 0 0 0 :<++> (StrokeColour 0 0 1 :<++> StrokeWidth 0.6), Translate 12 5, Circle 1)]"
        H.input ! A.type_ "submit" ! A.value "Submit"

---------------------------------------------------------------------------------------------------------------------------------------
{-|
  SVG Generator function.
  Essentially is just adds the proper SVG head onto the defined shape from the user input.
-}

customSvg' :: String -> S.Svg
customSvg' draw = S.docTypeSvg ! SVGA.version "1.1" ! SVGA.width "100%"
                        ! SVGA.height "100%" ! SVGA.viewbox "-25 -5 50 50"
                        $ S.g $ toSvg (read draw :: Drawing)

