
\version "2.19.21"

\header {
  texidoc = "unpure-pure containers take two arguments: an unpure property and
a pure property.  The pure property is evaluated (and cached) for all
pure calculations, and the unpure is evaluated for all unpure calculations.
In this regtest, there are three groups of two eighth notes.  In the first
group, the second note should move to accommodate the flag, whereas it should
not in the second group because it registers the flag as being higher. The
flag, however, remains at the Y-offset dictated by ly:flag::calc-y-offset.
In the third set of two 8th notes, the flag should be pushed up to a Y-offset
of 8.
"
}

\relative {
  \stemUp \autoBeamOff
  d'8 eis'
  \once \override Flag.Y-offset =
    #(ly:make-unpure-pure-container ly:flag::calc-y-offset 8)
  d,8 eis'!
  \once \override Flag.Y-offset = #8
  d,8 eis'!
}
