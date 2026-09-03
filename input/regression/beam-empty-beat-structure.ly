\version "2.26.1"


\header {

  texidoc = "@code{beatStructure} may be empty while a time signature
is in force.  With no beats for a beamlet to point at, beamlet
direction follows rhythmic importance, matching the metered rendering
of the same rhythm.  Automatic beaming is unaffected." }


\layout {
  indent = 1
  ragged-right = ##t
}

\score {
  \new Staff { c''16[ c'' c''8] c''16[ c'' c'' c''8] }
  \layout { \context { \Score timeSignature = ##f } }
}

\score {
  \new Staff {
    \omit Staff.TimeSignature
    \set Timing.beatStructure = #'()
    c''16[ c'' c''8] c''16[ c'' c'' c''8]
  }
}

\score {
  \new Staff {
    \omit Staff.TimeSignature
    \unset Timing.beatStructure
    c''16 c'' c''8 c''16 c'' c'' c''8
  }
}
