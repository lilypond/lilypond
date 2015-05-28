\version "2.19.21"

\header {
  texidoc = "In tuplets with an even number of stems, the number
may be placed on either side of the beam when the central stems
point in different directions.  The exception to this is when
there is a fractional beam on one of the central stems, in which
case the number is placed opposite the partial beam."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative {
  \time 2/4
  \override Beam.auto-knee-gap = 1
  \tuplet 6/4 4 {
    c'16 c'' c,, c'' c,, c''
    \once \override TupletNumber.direction = #UP
    c,,16 c'' c,, c'' c,, c''
  }
  \time 6/16
  \tuplet 4/3 8. {
    c,,8. c''16
    %% The following override has no effect:
    \override TupletNumber.direction = #DOWN
    c,,16 c''8.
  }
}
