{-
 - ***
 -
 - This module tests the functionality of the `Curves`-library
 -
 - I don't really know how to define good haskell-style test-cases I think.
 -}
module Tests where

import Curves

tests = [
	-- Don't know how to check if an it is possible to define a point
	Point (0,0) == Point (0,0),
	-- Check that the function point exists (also checks the equality-function)
	point (2,3) == Point (2,3),
	-- Check that fuzzy-comparison works
	Point (2,3) == Point (2.01, 2.99),
	Point (2,3) /= Point (1, 2),
	-- Curve is a type. Likewise I don't know how to check that you can define it.
	Curve [] == Curve [],
	-- Check that the connect-function works as expected.
	let empty = Curve [] in connect empty empty == empty,
	let
		empty = Curve []
		something = Curve (map Point [(0,4),(0,5)])
	in  (connect empty something) == something,
	let
		something         = Curve (map Point[(0,0)])
		somethingElse     = Curve (map Point[(0,1)])
		thoseTwoConnected = Curve (map Point[(0,0), (0,1)])
	in (connect something somethingElse) == thoseTwoConnected,
	-- Check that the rotate-function works
	let
		aCurve      = Curve [Point (1,1)]
		thatRotated = Curve [Point (1,-1)]
	in (rotate aCurve 90) == thatRotated,
	let
		aCurve      = Curve [Point (1,1)]
		thatRotated = Curve [Point (sqrt 2, 0)]
	in (rotate aCurve 45) == thatRotated,
	let
		aCurve         = Curve [Point (0, 0), Point (0,1)]
		translater     = Point (0,1)
		thatTranslated = Curve [Point (0,1), Point (0,2)]
	in translate aCurve translater == thatTranslated,
	let
		aCurve        = Curve [Point (1,1)]
		thatReflected = Curve [Point (1,-1)]
	in reflect aCurve Horizontal 0 == thatReflected,
	let
		aCurve   = Curve (map Point [(4,5),(7,9),(6,5),(2,5),(3,3)])
		itsBbox  = (Point (2,3), Point (7,9))
	in bbox aCurve == itsBbox
	]
