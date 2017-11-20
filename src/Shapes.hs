--{-# LANGUAGE OverloadedStrings #-}
module Shapes(
  Shape, Transform, Drawing,
  empty, circle, square,
  identity, translate, rotate,
  scale, toSvg,
  buildCustomSvgFromString)  where

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Colour.SRGB
import Text.Printf (printf)
import Text.Blaze.Svg11 ((!))

{-|
  Shape
  Basic Shape type and constructors supported by SVG
-}
data Shape = Empty
           | Circle Int
           | Square Int Int
             deriving (Show, Read)
--empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

{-|
  Style
  Basic styles supported by SVG.
  Compose here is fined as an infix constructer at the type level and as a distinct type
  This is to aid in typing something readable that can be easily derived from a read instance
-}
data Style = NoStyle
           | StrokeWidth Double
           | StrokeColour Double Double Double
           | FillColour Double Double Double
           | Compose' Style Style
           | Style :<++> Style
             deriving (Show, Read)
nostyle = NoStyle
strokeWidth   = StrokeWidth
strokeColour  = StrokeColour
fillColour    = FillColour
t0 <++> t1    = Compose' t0 t1
t0 :<++> t1   = Compose' t0 t1

{-|
  Transforms
  Basic transfroms supported by SVG.
-}
data Transform = Identity
           | Translate Double Double
           | Scale Double Double
           | Compose Transform Transform
           | Rotate Int
             deriving (Show, Read)
identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

{-|
  Styles and transformations are functionaly different in their implmentations
  as one "transformation" needs to be composed of all the composed parts whereas
  each style needs to be seperated. Hence the seperation between styles and
  transforms
-}
type Drawing = [(Style, Transform,Shape)]

{-|
  transformsToStrings
  This is a helper function for transformBuilder.
  The entire composed "list" of transforms needs to be folded into one
  S.Attribute for transforms to work correctly.
  Having failed to find a way to properly chain S.AttributeValues,
  I used the Blaze-Internal method to construct my own.

  Compose here is simple to implement as the strings only need
  to be concatenated to make valid syntax
-}
transformsToStrings :: Transform -> String
transformsToStrings (Translate x y)   = "translate(" ++ show x ++ " " ++ show y ++ ") "
transformsToStrings (Scale x y)       = "scale(" ++ show x ++ " " ++ show y ++ ") "
transformsToStrings Identity          = "scale(1 1) "
transformsToStrings (Rotate x )       = "rotate(" ++ show x ++ ") "
transformsToStrings (Compose t1 t2)   = transformsToStrings t1 ++ " " ++ transformsToStrings t2

{-|
  transformBuilder
  Main interpretation function in creating the transforms for an SVG.
-}
transformBuilder :: Transform -> S.Attribute
transformBuilder x = A.transform $ I.stringValue $ transformsToStrings x

-- Style Interpratations

{-|
  collourAttrVal
  sRGB uses 3 doubles representing RGB values to make a colour. Using Blaze-Internal
  I pull out the hex value as a valid colour for a style attribute
-}
colourAttrVal :: Double -> Double -> Double -> S.AttributeValue
colourAttrVal r g b = I.stringValue $ sRGB24show $ sRGB r g b

{-|
  strokeWidthAttrVal
  Boring helper function because I like typing more I suppose.
  Takes a double and turns to into an AttributeValue to be passed
  to an expecting attribute
-}
strokeWidthAttrVal :: Double -> S.AttributeValue
strokeWidthAttrVal d = I.stringValue $ show d

{-|
  createAttrib
  This is a helper function to translate a style to it's correspinding
  attribute
-}
createAttrib :: Style -> S.Attribute
createAttrib NoStyle              = A.name $ I.stringValue "Nothing"
createAttrib (FillColour r g b)   = A.fill $ colourAttrVal r g b
createAttrib (StrokeWidth d)      = A.strokeWidth $ strokeWidthAttrVal d
createAttrib (StrokeColour r g b) = A.stroke $ colourAttrVal r g b

{-|
  translateToListStyles
  Style attributes are a little different to the transform attributes
  as they must all be seperate.
  Most noteable difference here is in how compose is implmented. To make
  a valid [S.Attribute] the compose function is lifted out of each side.
  The individual components are just returned to be added to the list.
  In this way we can allow any number of composes'.
-}
translateToListStyles :: Style -> [Style]
translateToListStyles x@NoStyle              = [x]
translateToListStyles x@(FillColour r g b)   = [x]
translateToListStyles x@(StrokeWidth d)      = [x]
translateToListStyles x@(StrokeColour r g b) = [x]
translateToListStyles (Compose' s1 s2)       = translateToListStyles s1 ++ translateToListStyles s2

{-|
  styleBuilder
  Main interpretation function used to generate valid attributes from the style
-}
styleBuilder :: Style -> [S.Attribute]
styleBuilder x = map createAttrib $ translateToListStyles x

{-|
  createShapeAttrib
  Generates valid SVG head for a desired shape.
  They are initialised here with a default size of 1 and can be scaled to any size.
-}
createShapeAttrib :: Shape -> S.Svg
createShapeAttrib Empty         = S.rect ! A.r (I.stringValue "0")
createShapeAttrib (Circle r)    = S.circle ! A.r (I.stringValue (show r))
createShapeAttrib (Square w h)  = S.rect ! A.width (I.stringValue (show w)) ! A.height (I.stringValue (show h))

--
-- SVG GENERAL MAKE
--

{-|
  buildCustomSvgFromString
  EXPORTED
  Interface to allow the return of an SVG given in string representation in the Shape lanuage
-}
buildCustomSvgFromString :: String -> String -> String -> S.Svg
buildCustomSvgFromString style trans shape = toSvg drawing
                                    where drawing = [(read style :: Style, read trans :: Transform, read shape :: Shape)]
--read "[(Compose' (Compose' (FillColour 1 0.5 0) (StrokeColour 0 0 0)) (StrokeWidth 0.1), Compose (Rotate 5) (Scale 50 50), Square)]" :: Drawing
{-|
  toSvg
  EXPORTED
  Creates a valid SVG from the components of a drawing
-}
toSvg :: Drawing -> S.Svg
toSvg ((style, trans, shape):rest) = foldl (!) (createShapeAttrib shape) attributeList
                                    where
                                      attributeList = transformBuilder trans : styleBuilder style

-- TODO: Allow mutlipe drawings
-- TODO: Make form better
-- TODO: Add more shapes


