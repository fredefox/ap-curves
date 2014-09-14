# Advanced Programming Assigmnet 1

Written by Frederik Hanghøj Iversen
for the course Advanced Programming
at Copenhagen University 2014

This report is for the first assignment in the course Advanced Programming.

This docuement is written in [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown). It may best be viewed in the markdown-format but is also included as a pdf. Pdf-version created with from markdown with [pandoc](http://johnmacfarlane.net/pandoc/).

The project is maintened with git. Access can be granted by finding [me on GitHub](https://github.com/fredefox)

In this report I present the results of my efforts to implement the `Curves`-library
which can be found on the [course web page](http://www.diku.dk/~kflarsen/ap-2014/curves/curves.html) (henceforth "the specification").

Source-code is attached along with pdf and markdown-versions of this report.

# Introduction
This is the first code I have ever written in Haskell. Perhaps my code illustrates this. My own thinknig is that the main tell probably is that I use a limited set of concepts from the programming language.

## Code structure
The code is organized as follows:

 * `Curves.hs`   - Contains the main implementaiton of the library functions.
 * `Tests.hs`    - Contains tests-cases
 * `Export.hs`   - Contains functions to export to svg and files
 * `Examples.hs` - Contains an application of the library that produces hilbert-curves

The implementaiton of the library is in `Curves.hs` all functions are annotated with comments.

## How to use the source-code
### Reading it
Each bullet in the specification corresponds to a comment in the source-code that start with:

```haskell
{-
 - ***
 -
 - ...
 -
 -}
```

Multiple times in the code I have chosen to make implementations that differ a bit. These implementations have been marked with a single-quote (prime). Reasons for doing this could be that the my implementations is more in line with the mathematical definition or to create a looser coupling between the elements of the library or even to be able to reuse some of the code other places.

All functions that are mentioned in the specification have reference implementations here with corresponding names.

To take an example. `reflect'` does exactly what the name suggests, whereas `reflect` is both a reflection and a translation. That is also how `reflect` has been implemented - by using `reflect'` and `translate`.


### Running it
To test the source-code fire up `ghci` and load the library with:

```
ghci> :l Curves.hs
```

To see the result of the test-cases do:

```
ghci> :l Tests.hs
ghci> tests
```

To see the hilbert-curve drawn out do:

```
ghci> :l Examples.hs
ghci> showHilbert "hilbert"
```

A file is now created with the name `hilbert.svg`. This is just a small convenience method to produce the example from the assignment text.

# The code
## Mathematical objects using `newtype`, and `data`
I have chosen to implement the different mathematical concepts "Point", "Curve" and "Axis" as types using the keyword `newtype`. This led me to the problem that they no longer behaved like tuples or lists of tuples (which they essentially just are). To deal with this problem the functions `unPoint` and `unCurve` are defined to give access to what information they hold. This has the downside that some of the code is more verbose than it needs be because `Curve`s and `Point`s needs to be packed and unpacked now and again.

## Transformations using `class`
I have also chosen to define the transformations (`translate`, `reflect`, `rotate`) for both `Point`s and `Curve`s. This gave the benefit that the definition of rotating a curve basically boils down to rotating all the point on the curve.

The specifications hints at defining a curve as something containing a field `startPoint` or some such. I have chosen just to make it a list and interpret the starting-point of a curve to be the head-element in the underlying list. So the required definition of `curve` (given the types seen in the specification) becomes:

```haskell
curve :: Point -> [Point] -> Curve
curve sp ps = Curve (sp:ps)
```

## Code assessment
`Eq` has been derived for both `Point` and `Curve` to enable testing.

The test-cases that I have written can be found in `Tests.hs`. They are a series of test-cases, each covering some aspect of the code. There is at least one test for each bullet in the specification (except for `toList`, which is basically just `unCurve`)

After I was done implementing the library and the svg-export features and I ran the hilbert-function I received the output found in "hilbert-wrong.svg". I then went on to start defining the test-cases while going through the specification again. Only then when I came to the function `rotate` did I see that I had implemented a rotation in the standard counter-clockwise fashion whereas the specification asked for a counter-clockwise rotation. I fixed it, and the result with respects to the hilbert-curve can be seen in "hilbert-correct.svg". This error would have been difficult to weed out had it not been for the test-cases.

Here are an excerpt of some of the test-cases:

```haskell
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
```

The included test-cases all cover one of the required transformations. All test-cases pass.

I believe that the test-cases along with the demonstrated functionality of the hilbert-function nicely shows that the library works.

As to the quality of the code itself. As previously mentioned, I would like to improve the code so that calls between `unCurve` and `Curve` would not be neccessary. But as of this time I do not know how to acheive this.

I also particularly like the fact that I have made employed "ad-hoc polymorphism" in my implementations of the transformations.

One missing feature in the `Export`-module is the ability to handle the case when points have negative coordinates. The svg-format does not allow for the drawing of such. Therefore to draw a curve it must first be translated so that all coordinates have positive values. Currently if you for instance try to draw the line: "(0,0) -> (-1,-1)" you will get a drawing that has a height and a width of "1", yet the line will extend up and to the left - that is off the svg-canvas.

# Conclusion
An implementation of the `Curves`-library has been presented including source-code and comments herein.

The code has been shown to work as expected through test-cases as well as in the application of drawing hilbert-curves. All functions meet requirements of the official specification. Some of the definitions in the specification are less appropriate. For these alternative functions (marked with a prime) have been implemented to allow for greater flexibility and extensibility of the library.

The code could be improved by refactoring it and making it more readable. The way it is implemented allows for extensibility.
