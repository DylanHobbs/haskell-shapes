module Shapes(
  Shape, Transform, Drawing,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>), toSvg, roatedSquare, buildCustomSvgFromString)  where

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Colour.SRGB
import Text.Printf (printf)
import Text.Blaze.Svg11 ((!))

data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square


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


type Drawing = [(Style, Transform,Shape)]
{-|
  Styles and transformations are functionaly different in their implmentations
  as one "transformation" needs to be composed of all the composed parts whereas
  each style needs to be seperated.
-}

transformsToStrings :: Transform -> String
transformsToStrings (Translate x y)   = "translate(" ++ show x ++ " " ++ show y ++ ") "
transformsToStrings (Scale x y)       = "scale(" ++ show x ++ " " ++ show y ++ ") "
transformsToStrings Identity          = "scale(1 1) "
transformsToStrings (Rotate x )       = "rotate(" ++ show x ++ ") "
transformsToStrings (Compose t1 t2)   = transformsToStrings t1 ++ " " ++ transformsToStrings t2

transformBuilder :: Transform -> S.Attribute
transformBuilder x = A.transform $ I.stringValue $ transformsToStrings x

-- Style Interpratations
-- Translate colour to valid hex ()
colourAttrVal :: Double -> Double -> Double -> S.AttributeValue
colourAttrVal r g b = I.stringValue $ sRGB24show $ sRGB r g b

-- Translate double to valid width value (0-MAX_INT)
strokeWidthAttrVal :: Double -> S.AttributeValue
strokeWidthAttrVal d = I.stringValue $ show d

-- Creates an SVG Attribute to be passed to the construction
createAttrib :: Style -> S.Attribute
createAttrib NoStyle              = A.name $ I.stringValue "Nothing"
createAttrib (FillColour r g b)   = A.fill $ colourAttrVal r g b
createAttrib (StrokeWidth d)      = A.strokeWidth $ strokeWidthAttrVal d
createAttrib (StrokeColour r g b) = A.stroke $ colourAttrVal r g b

translateToListStyles :: Style -> [Style]
translateToListStyles x@NoStyle              = [x]
translateToListStyles x@(FillColour r g b)   = [x]
translateToListStyles x@(StrokeWidth d)      = [x]
translateToListStyles x@(StrokeColour r g b) = [x]
translateToListStyles (Compose' s1 s2)       = translateToListStyles s1 ++ translateToListStyles s2

styleBuilder :: Style -> [S.Attribute]
styleBuilder x = map createAttrib $ translateToListStyles x

-- Creates the head shape SVG attribute apply whenever you want a shape
createShapeAttrib :: Shape -> S.Svg
createShapeAttrib Empty   = S.rect ! A.r (I.stringValue "0")
createShapeAttrib Circle  = S.circle ! A.r (I.stringValue "10")
createShapeAttrib Square  = S.rect ! A.width (I.stringValue "10") ! A.height (I.stringValue "10")

--
-- SVG GENERAL MAKE
--

genTransStuff :: Drawing -> [Transform]
genTransStuff [] = []
genTransStuff ((style, trans, shape):ds) = trans : genTransStuff ds

genSvgStuff :: Drawing -> [S.Attribute]
genSvgStuff [] = []
genSvgStuff ((style, trans, shape):ds) = createAttrib style : genSvgStuff ds

-- Svg Builder
-- Utility builder from string
buildCustomSvgFromString :: String -> String -> String -> S.Svg
buildCustomSvgFromString style trans shape = toSvg drawing
                                    where drawing = [(read style :: Style, read trans :: Transform, read shape :: Shape)]

toSvg :: Drawing -> S.Svg
toSvg d@((style, trans, shape):rest) = do
  --let a = A.transform $ head $ genTransStuff d
  let t = transformBuilder trans
  let s = styleBuilder style
  let attribList = t : s
  foldl (!) (createShapeAttrib shape) attribList


-- Test Sections
-- TODO: Remove vectors
-- TODO: Create lists with where clause like I made with shane
-- TODO: Make a combine function for styles and tranforms
-- TODO: Combine styles and transforms

roatedSquare = toSvg s
  where s = [(strokeWidth 1, scale  2 2, square), (strokeColour 0 0 0, rotate 45, square), (fillColour 1 0 0, identity, square)]

test = toSvg s
  where s = [(strokeWidth 2 <++> strokeColour 0 0 0 <++> fillColour 1 0 0, Rotate 10 <+> Scale 2 2 <+> Identity, square)]
















