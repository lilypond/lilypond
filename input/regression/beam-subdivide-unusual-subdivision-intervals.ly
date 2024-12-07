\version "2.25.23"

\header {
  texidoc = "Properties @code{beamMaximumSubdivision} and
@code{beamMinimumSubdivision} may have a non-power of 2 numerator.  If
@code{beamMaximumSubdivision} does have non-power-of-2@tie{}
numerator, let @code{n} be the largest odd factor of that numerator.
The beamlets should be subdivided @code{n} times less frequently than
as if @code{n} was@tie{}1.  Since @code{beamMinimumSubdivision}
only sets a lower bound for intervals of subdivision interval, it having
a non-power-of-2@tie{}numerator is not much any different from an
approriate power-of-2@tie{}counterpart.
"
}


\paper {
  indent = 0
  ragged-right = ##t
}

first = {
  \repeat unfold 4 c256 c128
}
remaining = {
  \repeat unfold 3 \first \first c256 c c c c128^"pos=1/8" \repeat unfold 3 \first c256 c c c c32
  \break
}

\relative c' {
  \time 1/4
  \omit Staff.Clef


  c64^"unsubdivided" \remaining

  \set subdivideBeams = ##t
  c64^"default subdivision" \remaining

  \once \set beamMaximumSubdivision = #3/16
  c64^"max=3/16" \remaining

  \once \set beamMaximumSubdivision = #3/32
  c64^"max=3/32" \remaining

  \once \set beamMinimumSubdivision = #3/32
  c64^"min=3/32" \remaining

  \set beamMaximumSubdivision = #3/16
  \once \set beamMinimumSubdivision = #2/128
  c64^"max=3/16 min=2/128" \remaining

  \once \set beamMinimumSubdivision = #3/128
  c64^"max=3/16 min=3/128" \remaining

  \once \set beamMinimumSubdivision = #4/128
  c64^"max=3/16 min=4/128" \remaining
}

first = {
  \repeat unfold 4 c16 c8
}
remaining = {
  c8 \repeat unfold 4 \first c16 c c c c8^"pos=2" \repeat unfold 4 \first s4.
  \break
}

\relative c' {
  \time 4/1
  \omit Staff.Clef

  c8^"unsubdivided" \remaining

  \set subdivideBeams = ##t
  c8^"default subdivision" \remaining

  \once \set beamMaximumSubdivision = #3/2
  c8^"max=3/2" \remaining

  \once \set beamMinimumSubdivision = #3/2
  c8^"min=3/2" \remaining

  \set beamMaximumSubdivision = #3/2
  \once \set beamMinimumSubdivision = #2/2
  c8^"max=3/2 min=2/2" \remaining

  \once \set beamMinimumSubdivision = #3/2
  c8^"max=3/2 min=3/2" \remaining

  \once \set beamMinimumSubdivision = #4/2
  c8^"max=3/2 min=4/2" \remaining
}
