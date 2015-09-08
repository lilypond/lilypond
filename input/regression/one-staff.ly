\version "2.19.26"

\header {
  texidoc = "@code{OneStaff} contexts can be used for letting several
contexts use the same vertical position.  This example shows chords being
placed in a staff and immediately following it."
}

\layout {
  ragged-right = ##t
}

\new OneStaff
{
  << % First element in <<>> so that it is not kept alive spuriously
    \new Staff
    {
      c'4 d' e' f'
      \chords \with { \override ChordName.Y-offset = -1 }
      { d1:m7 b1:min7.5- }
    }
  >>
  \chords \with { \override ChordName.Y-offset = -1 }
  { d1:m7 b1:min7.5- }
}
