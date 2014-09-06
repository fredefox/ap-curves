{-
 - Definition of a point with constructor and decontructor
 -}
newtype Point = Point { unPoint :: (Double, Double) } deriving (Show)
-- QA: How can this definition infer that the constructor takes a tuple of two points?

-- Because the specification asks for a constructor called `point'
point = Point


{-
 - Define equality for points to be fuzzy
 -}
instance Eq Point where
  a == b = True
  a /= b = False
  -- TODO: Not done

{-
 - Definition of `Curve'
 -}
newtype Curve = Curve { unCurve :: [Point] } deriving (Show)

-- Again the specification asks for a lower-case constructor `point'
curve = Curve

{-
 - Definition of connect
 -}
connect :: Point -> [Point] -> Curve
connect p [] = Curve [p]
connect p ps = Curve (p : ps)

{-
 - Definition of rotate
 -}
--rotate :: Curve -> Double -> Curve




class Rotatable a where
	rotate :: a -> Double -> a

instance Rotatable Point where
	rotate p theta = Point (x', y')
		where
			(x, y) = unPoint p
			x' = x * cos theta' - y * sin theta'
			y' = x * sin theta' + y * cos theta'
			theta' = theta * pi / 180

instance Rotatable Curve where
	rotate ps theta = Curve [ rotate p theta | p <- unCurve ps ]

{-
 - Definition of distance between two points
 -
 - I'd like to define this as a type-class [see further down]
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
 - This method-definition is weird.
 - `Point' is analogous to a vector
 - Something translated by a vector would normally be translated
 - a distance according to the vector, but here we arbitrarily define
 - some 'starting point' of a curve and the translation is then the translation of 
 - the curve in relation to vector between the starting point and the actual parameter.
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

data Axis = Vertical | Horizontal deriving (Eq)

{-
 - Reflection here is also weird, it's really actually a reflection *and* a translation
 -}
class Reflectable' a where
	reflect' :: a -> Axis -> a

instance Reflectable' Point where
	reflect' a axis
		| axis == Vertical   = Point (-a1,  a2)
		| axis == Horizontal = Point ( a1, -a2)
		where (a1, a2) = unPoint a

instance Reflectable' Curve where
	reflect' crv axis = Curve [ reflect' p axis | p <- unCurve crv ]

{-
 - Now for the desired `reflect` it's important to do the translation after the reflection
 -}
class Reflectable a where
	reflect :: a -> Axis -> Double -> a

instance Reflectable Point where
	reflect p axis delta = translate' (reflect' p axis) q
		where q
			| axis == Horizontal = Point (0, delta)
			| axis == Vertical   = Point (delta, 0)

instance Reflectable Curve where
	reflect crv axis delta = Curve [ reflect p axis delta | p <- unCurve crv ]

bbox :: Curve -> (Point, Point)
bbox crv = (Point (minX, minY), Point (maxX, maxY)) where
	(minX, maxX) = (minimum xs, maximum xs)
	(minY, maxY) = (minimum ys, maximum ys)
	xs = [ fst tpl | x <- unCurve crv, let tpl = unPoint x ]
	ys = [ snd tpl | x <- unCurve crv, let tpl = unPoint x ]

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

toList :: Curve -> [Point]
toList = unCurve
