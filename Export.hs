module Curves.Export where

-- The stuff we can export
import Curves
-- Used for formatting
import Text.Printf

{-
 - ***
 -
 - `toSvg`
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
			(lines (unCurve crv)) ++
		"</g>" ++
	"</svg>"
		where
			lines ps = concat [
				segment a b |
					(i, e) <- zip [0..] ps,
					let a = unPoint (ps !! i),
					let b = unPoint (ps !! ( i + 1 )),
					i < length ps - 1
				]
			segment a b = printf s (fst a) (fst b) (snd a) (snd b)
			s     = "<line style=\"stroke-width: 2px; stroke: black; fill:white\" " ++
				"x1=\"%.2f\" x2=\"%.2f\" y1=\"%.2f\" y2=\"%.2f\"/>"

-- Abbreviations should be treated like words in CamelCase -
-- only have their first letter be upper-case
toSVG = toSvg

{-
 - ***
 -
 - `toFile`
 -
 -}
toFile :: Curve -> FilePath -> IO ()
toFile crv fp = writeFile fp (toSvg crv)

aCurve = curve (point (0,100)) (map point [(500,200),(100,100),(0,100)])
main = toFile aCurve "test.svg"
