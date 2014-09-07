{-
 - ***
 -
 - This module shows example curves made with the `Curve`-library.
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


module Examples where

import Curves
import Export
{-
 - ***
 -
 - Example curves with the `Curves`-library
 -
 -}
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 10

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` (point (w+p+w, h+p+h))
          c1 = c `translate` (point (w+p+w, h))
          c2 = c
          c3 = ch `rotate` 90 `translate` (point (0, h+p))
