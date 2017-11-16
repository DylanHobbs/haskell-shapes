module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square,
  identity, translate, rotate, scale, (<+>),
  inside)  where

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Svg11.Attributes as A
import Data.Colour.SRGB
import Text.Printf (printf)
import Text.Blaze.Svg11 ((!))

-- Utilities
data Style = StrokeWidth Double
--           | StrokeColour Int Int Int
           | FillColour Int Int Int
        deriving (Show, Read)

data Vector = Vector Double Double
              deriving Show

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
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p


-- Drawings

type Drawing = [(Style,Transform,Shape)]

colourAttrVal :: Int -> Int -> Int -> S.AttributeValue
colourAttrVal r g b = I.stringValue $ sRGB24show $ sRGBBounded r g b

strokeWidthAttrVal :: Double -> S.AttributeValue
strokeWidthAttrVal d = I.stringValue $ show d

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Style, Transform, Shape) -> Bool
inside1 p (st,t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

createAttrib :: Style -> S.Attribute
createAttrib (FillColour r g b) =   A.fill $ colourAttrVal r g b
createAttrib (StrokeWidth d)    =   A.strokeWidth $ strokeWidthAttrVal d

createShapeAttrib :: Shape -> S.Svg
createShapeAttrib Empty = S.rect ! A.r (I.stringValue "0")
createShapeAttrib Circle = S.circle ! A.r (I.stringValue "2")
createShapeAttrib Square = S.rect ! A.width (I.stringValue "1") ! A.height (I.stringValue "2")

genSvgStuff :: Drawing -> [S.Attribute]
genSvgStuff [] = []
genSvgStuff ((style, trans, shape):ds) = createAttrib style : genSvgStuff ds

toSvg :: Drawing -> S.Svg
toSvg d@[(style, trans, shape)] = foldl (!) (createShapeAttrib shape) $ genSvgStuff d

testShape = (scale (point 10 10), circle)

testFillRect = (FillColour 10 10 10, scale(point 10 10), square)
