module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>), toSvg, colouredSquare, colouredCircle, blockyCircle, roatedSquare, buildCustomSvgFromString)  where

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Colour.SRGB
import Text.Printf (printf)
import Text.Blaze.Svg11 ((!))

-- Utilities
data Style = StrokeWidth Double
           | StrokeColour Double Double Double
           | FillColour Double Double Double
        deriving (Show, Read)
fillColour = FillColour
strokeColour = StrokeColour
strokeWidth = StrokeWidth

data Vector = Vector Double Double
              deriving Show

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

vector = Vector

type Shading = Float

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty
           | Circle
           | Square
             deriving (Show, Read)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

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

--transform :: Transform -> Point -> Point
--transform Identity                   x = id x
--transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
--transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
--transform (Rotate m)                 p = (invert m) `mult` p
--transform (Compose t1 t2)            p = transform t2 $ transform t1 p


-- Drawings

type Drawing = [(Style,Transform,Shape)]

-- interpretation function for drawings

--inside :: Point -> Drawing -> Bool
--inside p d = or $ map (inside1 p) d
--
--inside1 :: Point -> (Style, Transform, Shape) -> Bool
--inside1 p (st,t,s) = insides (transform t p) s
--
--insides :: Point -> Shape -> Bool
--p `insides` Empty = False
--p `insides` Circle = distance p <= 1
--p `insides` Square = maxnorm  p <= 1
--
--distance :: Point -> Double
--distance (Vector x y ) = sqrt ( x**2 + y**2 )
--
--maxnorm :: Point -> Double
--maxnorm (Vector x y ) = max (abs x) (abs y)



-- --Interpratation functions for SVG

--
-- Interpretation functions to allow original transforms in SVG
--

---- Return transformaion parts to be built once complete
--createTransAttrib :: Transform -> S.AttributeValue
--createTransAttrib (Translate (Vector x y)) = S.translate x y
--createTransAttrib (Scale (Vector x y)) = S.scale x y
--createTransAttrib Identity = S.scale 1 1
--createTransAttrib (Rotate x) = S.rotate x

transformsToStrings :: [Transform] -> String
transformsToStrings [] = []
transformsToStrings (Translate x y:rest) =  ("translate(" ++ show x ++ " " ++ show y ++ ") ") ++ transformsToStrings rest
transformsToStrings (Scale x y:rest) = ("scale(" ++ show x ++ " " ++ show y ++ ") ") ++ transformsToStrings rest
transformsToStrings (Identity:rest) = "scale(1 1) " ++ transformsToStrings rest
transformsToStrings (Rotate x : rest) = ("rotate(" ++ show x ++ ") ") ++ transformsToStrings rest

transformBuilder :: [Transform] -> S.AttributeValue
transformBuilder x = I.stringValue $ transformsToStrings x



--
-- COLOUR AND SHADE
--

-- Translate colour to valid hex ()
colourAttrVal :: Double -> Double -> Double -> S.AttributeValue
colourAttrVal r g b = I.stringValue $ sRGB24show $ sRGB r g b

-- Translate double to valid width value (0-MAX_INT)
strokeWidthAttrVal :: Double -> S.AttributeValue
strokeWidthAttrVal d = I.stringValue $ show d

-- Creates an SVG Attribute to be passed to the construction
createAttrib :: Style -> S.Attribute
createAttrib (FillColour r g b)   = A.fill $ colourAttrVal r g b
createAttrib (StrokeWidth d)      = A.strokeWidth $ strokeWidthAttrVal d
createAttrib (StrokeColour r g b) = A.stroke $ colourAttrVal r g b

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
  let s = A.transform $ transformBuilder $ genTransStuff d
  let b = genSvgStuff d
  let attribList = s : b
  foldl (!) (createShapeAttrib shape) attribList


-- Test Sections
-- TODO: Remove vectors
-- TODO: Create lists with where clause like I made with shane
-- TODO: Make a combine function for styles and tranforms
-- TODO: Combine styles and transforms

roatedSquare = toSvg s
  where s = [(strokeWidth 1, scale  2 2, square), (strokeColour 0 0 0, rotate 45, square), (fillColour 1 0 0, identity, square)]

colouredSquare shape r g b = toSvg s
  where s = [(fillColour r g b, identity, square)]

colouredCircle shape r g b = toSvg s
  where s = [(fillColour r g b, identity, circle)]

blockyCircle d = toSvg s
  where s = [(strokeWidth d, identity, circle), (strokeColour 0 0 0, identity, circle), (fillColour 1 0 0, identity, circle)]

