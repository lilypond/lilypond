\version "2.23.10"

\header {
  lsrtags = "chords"

  texidoc = "
Custom divisions of chord squares can be defined through the
@code{measure-division-lines-alist} and @code{measure-division-chord-placement-alist}
properties of @code{ChordSquare}.  These are both alists.  Their keys are
measure divisions, namely lists which give the fraction of the measure
that each chord (or rest, or skip) represents.  More precisely, a measure
division alist is made of positive, exact numbers adding up to 1, for
example: @code{'(1/2 1/4 1/4)}.  The exactness requirement means that,
e.g., @code{1/2} is valid but not @code{0.5}.

The values in @code{measure-division-lines-alist} are lists of lines,
which are represented as @code{(@var{x1} @var{y1} @var{x2} @var{y2})}.
The line starts at the point @code{(@var{x1} . @var{y1})} and ends at
@code{(@var{x2} . @var{y2})}.  Coordinates are expressed in the
[-1,@tie{}1] scale relative to the extent of the square.

The values in @code{measure-division-chord-placement-alist} are
lists of @code{(@var{x} . @var{y})} pairs giving the placement of
the respective chords.

This example defines a peculiar chord grid style that has a rule
for measures divided in three equal parts.
"

  doctitle = "Customizing the chord grid style"
}


\paper {
  line-width = 10\cm
  ragged-right = ##f
}

\new ChordGrid \with {
  \override ChordSquare.measure-division-lines-alist =
    #'(((1) . ())
       ((1/3 1/3 1/3) . ((-1 -0.4 0 1) (0 -1 1 0.4))))
  \override ChordSquare.measure-division-chord-placement-alist =
    #'(((1) . ((0 . 0)))
       ((1/3 1/3 1/3) . ((-0.7 . 0.5) (0 . 0) (0.7 . -0.5))))
}
\chordmode {
  \time 3/4
  c2.
  c4 c4 c4
}
