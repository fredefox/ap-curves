{-
 - ***
 -
 - This module implements all the functions of the curve-library.
 -
 - The implementation follows the specification located at:
 -
 -     [advanced programming course homepage](http://www.diku.dk/~kflarsen/ap-2014/curves/curves.html)
 -
 - Multiple times in the code I have chosen to make implementations that differ a bit.
 - These implementations have been marked with a tick-symbol.
 - Reasons for doing this could be that the my implementations is more in
 - line with the mathematical definition or to create a looser coupling
 - between the elements of the library.
 -
 - All functions mentioned in the specification have reference implementations here.
 -
 - This library defines the following symbols:
 -
 -  * types:
 -    * Point
 -    * Curve
 -    * Axis
 -  * transformations:
 -    * rotate
 -    * translate
 -    * reflect
 -  * other functions:
 -    * bbox
 -
 - Written by Frederik Hanghøj Iversen
 - for the course Advanced Programming
 - at The University of Copenhagen 2014
 -
 - me@fredefox.eu /^._
 -  ,___,--~~~~--' /'~
 -  `~--~\ )___,)/'
 -      (/\\_  (/\\_
 -
 -}
module Curves where


{-
 - ***
 -
 - Definition of a point with constructor and decontructor
 -
 -}
newtype Point = Point { unPoint :: (Double, Double) } deriving (Show)
-- QA: How can this definition infer that the constructor takes a tuple of two points?

{-
 - ***
 -
 - Because the specification asks for a constructor called `point'
 -
 -}
point :: (Double, Double) -> Point
point = Point


{-
 - ***
 -
 - Define equality for points to be fuzzy
 -
 -}
instance Eq Point where
	a == b =
		b1 - 0.1 <= a1       &&
		a1       <  b1 + 0.1 &&
		b2 - 0.1 <= a2       &&
		a2       <  b2 + 0.1
		where
			(a1, a2) = unPoint a
			(b1, b2) = unPoint b

{-
 - ***
 -
 - Definition of `Curve'
 -
 -}
newtype Curve = Curve { unCurve :: [Point] } deriving (Show, Eq)

-- Again the specification asks for a lower-case constructor `point'
{-
 - The definition of `point` varies from what I initially thought it should be because
 - it aparantly takes a first argument which should be the first point on the curve.
 - I implement this by just letting the "starting-point" be the head of the
 - array of points (which is a curve) at any given time.
 -
 - So for instance a two curves
 -
 -     [s1, ...], [s2, ...]
 -
 - with respective starting points `s1` and `s2` joined together produces:
 -
 -     [s1, ...]
 -
 - Note that only one starting point is defined for this curve.
 -
 -}
curve :: Point -> [Point] -> Curve
curve sp ps = Curve (sp:ps)

{-
 - ***
 -
 - Definition of connect
 -
 -}
connect :: Curve -> Curve -> Curve
connect p q = Curve ((unCurve p) ++ (unCurve q))

{-
 - ***
 -
 - Definition of rotate
 -
 - This function conforms to the specification.
 -
 - It's a weird implementations in two regards:
 -  * The built-in functions `sin` and `cos` uses radians. This doesn't.
 -  * Normal rotation in mathematics is defined in the counter-clockwise direction. This isn't.
 -}
class Rotatable a where
	rotate :: a -> Double -> a

instance Rotatable Point where
	rotate p theta = Point (x', y')
		where
			(x, y) = unPoint p
			x' = x * cos theta' - y * sin theta'
			y' = x * sin theta' + y * cos theta'
			theta' = -theta * pi / 180

instance Rotatable Curve where
	rotate ps theta = Curve [ rotate p theta | p <- unCurve ps ]

{-
 - ***
 -
 - Definition of distance between two points
 -
 - I'd like to define this as a type-class [see further down]
 - TA Oleksandr says that the implementation would be out of scope
 -
 -}
distance :: Point -> Point -> Point
distance a b = Point (b1 - a1, b2 - a2) where
	(a1, a2) = unPoint a
	(b1, b2) = unPoint b

{-
class Measurable a b where
	distance :: a -> b -> Point

instance Measurable Point where
	distance a b = Point (b1 - a1, b2 - a2) where
		(a1, a2) = unPoint a
		(b1, b2) = unPoint b
-}

{-
 - ***
 -
 - The definition of Translate
 -
 - This method-definition is weird.
 - `Point' is analogous to a vector
 - Something translated by a vector would normally be translated
 - a distance according to the vector, but here we arbitrarily define
 - some 'starting point' of a curve and the translation is then the translation of 
 - the curve in relation to vector between the starting point and the actual parameter.
 -
 -}
-- This is the normal translate-function
translate' :: Point -> Point -> Point
translate' a b = Point (a1 + b1, a2 + b2) where
	(a1, a2) = unPoint a
	(b1, b2) = unPoint b

class Translatable a where
	translate :: a -> Point -> a

instance Translatable Point where
	translate a b = translate' a delta where
		delta = distance a b

instance Translatable Curve where
	translate crv a = Curve [ translate' x delta | x <- unCurve crv,
		let delta = distance (head (unCurve crv)) a ]

{-
 - ***
 -
 - Data-type `Axis`
 -
 -}
data Axis = Vertical | Horizontal deriving (Eq)

{-
 - ***
 -
 - Definition of `reflect`
 -
 - Reflection here is also weird, it's really actually a reflection *and* a translation
 -
 -}
class Reflectable' a where
	reflect' :: a -> Axis -> a

instance Reflectable' Point where
	reflect' a axis
		| axis == Vertical   = Point (-a1,  a2)
		| otherwise          = Point ( a1, -a2)
		where (a1, a2) = unPoint a

instance Reflectable' Curve where
	reflect' crv axis = Curve [ reflect' p axis | p <- unCurve crv ]

-- Now for the desired `reflect` it's important to do the translation after the reflection
class Reflectable a where
	reflect :: a -> Axis -> Double -> a

instance Reflectable Point where
	reflect p axis delta = translate' (reflect' p axis) q
		where q
			| axis == Horizontal = Point (0, delta)
			| otherwise          = Point (delta, 0)

instance Reflectable Curve where
	reflect crv axis delta = Curve [ reflect p axis delta | p <- unCurve crv ]

{-
 - ***
 -
 - `bbox`
 -
 -}
bbox :: Curve -> (Point, Point)
bbox crv = (Point (minX, minY), Point (maxX, maxY)) where
	(minX, maxX) = (minimum xs, maximum xs)
	(minY, maxY) = (minimum ys, maximum ys)
	xs = [ fst tpl | x <- unCurve crv, let tpl = unPoint x ]
	ys = [ snd tpl | x <- unCurve crv, let tpl = unPoint x ]
{-
 - ***
 -
 - `width` and `height`
 -
 -}
width  :: Curve -> Double
width crv = b - a where
	((a, _), (b, _)) = (lowerLeft, upperRight)
	lowerLeft  = unPoint (fst (bbox crv))
	upperRight = unPoint (snd (bbox crv))

height :: Curve -> Double
height crv = b - a where
	((_, a), (_, b)) = (lowerLeft, upperRight)
	lowerLeft  = unPoint (fst (bbox crv))
	upperRight = unPoint (snd (bbox crv))

{-
 - ***
 -
 - `toList`
 -
 -}
toList :: Curve -> [Point]
toList = unCurve
