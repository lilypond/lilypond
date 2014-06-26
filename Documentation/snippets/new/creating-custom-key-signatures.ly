\version "2.18.0"

\header {
  lsrtags = "tweaks-and-overrides, pitches, staff-notation, contexts-and-engravers"

  texidoc = "
LilyPond supports custom key signatures.  In this example, print for D
minor with an extended range of printed flats.
"
  doctitle = "Creating custom key signatures"
}

\new Staff \with {
  \override StaffSymbol.line-count = #8
  \override KeySignature.flat-positions = #'((-7 . 6))
  \override KeyCancellation.flat-positions = #'((-7 . 6))
  % presumably sharps are also printed in both octaves
  \override KeySignature.sharp-positions = #'((-6 . 7))
  \override KeyCancellation.sharp-positions = #'((-6 . 7))

  \override Clef.stencil = #
  (lambda (grob)(grob-interpret-markup grob
  #{ \markup\combine
    \musicglyph #"clefs.C"
    \translate #'(-3 . -2)
    \musicglyph #"clefs.F"
   #}))
    clefPosition = #3
    middleCPosition = #3
    middleCClefPosition = #3
}

{
  \key d\minor
  f bes, f bes,
}
