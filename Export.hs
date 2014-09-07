{-
 - ***
 -
 - This module allows for transforming the `Curve` type into an svg string-representation.
 - It also implements a convenience method that allows printing the `Curve` to an .svg-file.
 -
 - The implementation follows the specification located at:
 -
 -     [advanced programming course homepage](http://www.diku.dk/~kflarsen/ap-2014/curves/curves.html)
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
module Export where

-- The stuff we can export
import Curves
-- Used for formatting
import Text.Printf

{-
 - ***
 -
 - `toSvg`
 -
 - TODO: .svg-files cannot have negative coordinates.
 - All `Point`s should be translated so that there are no negative coordinates.
 -
 -}
toSvg :: Curve -> String
toSvg crv =
	"<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
		(printf
			"width=\"%dpx\" height=\"%dpx\" version=\"1.1\">"
			(ceiling (width crv)::Int)
			(ceiling (height crv)::Int)
		) ++
		"<g>" ++
			(lines' (unCurve crv)) ++
		"</g>" ++
	"</svg>"
		where
			lines' ps = concat [
				segment a b |
					(i, _) <- zip [0..] ps,
					let a = unPoint (ps !! i),
					let b = unPoint (ps !! ( i + 1 )),
					i < length ps - 1
				]
			segment a b = printf s (fst a) (fst b) (snd a) (snd b)
			s     = "<line style=\"stroke-width: 2px; stroke: black; fill:white\" " ++
				"x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\"/>"

-- Abbreviations should be treated like words in CamelCase -
-- only have their first letter be upper-case
toSVG :: Curve -> String
toSVG = toSvg

{-
 - ***
 -
 - `toFile`
 -
 -}
toFile :: Curve -> FilePath -> IO ()
toFile crv fp = writeFile fp (toSvg crv)
