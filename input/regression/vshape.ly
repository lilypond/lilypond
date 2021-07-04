\version "2.23.4"

\header {
  texidoc = "The @code{\vshape} command acts like the @code{\shape}
command, and additionally displays control points and polygons for
easier tweaking of the values.

The polygons are drawn on top of other notation, and the points
on top of the polygons."
}

testVshape = \vshape #'((0 . 0) (1 . 0) (2 . 0) (3 . 0)) \etc

\relative c' {
  % Test the tweak-like form.
  c1\testVshape~ c

  % Test the override-like form.
  \vshape #'((0 . 0) (1 . 0) (2 . 0) (3 . 0)) Slur
  c1\testVshape( c)

  % Test other kinds of bows.

  % \override works without specifying Score context
  % (even though the engraver actually lives in Score).
  \once \override ControlPolygon.thickness = 10
  c1\testVshape\( c\)

  c1\testVshape\laissezVibrer

  % Test concurrent curves and broken spanners.
  c1\repeatTie
  c1
    \vshape #'(((0 . 0) (0 . 0) (0 . 0) (0 . 3))
               ((0 . 0) (0 . 0) (0 . 0) (0 . -3)))
    ~
    \vshape #'(((0 . 0) (0 . 0) (0 . 0) (0 . 1))
               ((0 . 0) (0 . 0) (0 . 0) (0 . -2)))
    (
  \break
  c1 d1)
}
