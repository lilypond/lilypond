\version "2.21.0"

\header {
  texidoc = "Duration multipliers can be specified as scheme
expressions, either as rationals or as a moment."
}

musicwithdrone = \repeat unfold 2 { c'4 e' g' c'' }

musiclen = #(ly:music-length musicwithdrone)
musicrat = #(ly:moment-main musiclen)

\score {
  <<
    { \musicwithdrone \musicwithdrone
      \musicwithdrone \musicwithdrone \bar "|." }
    { c'1 * #(ly:music-length musicwithdrone) ~
      c'1 * \musiclen ~
      c'1 * \musicrat ~
      \scaleDurations \musiclen c'1
    }
  >>
  \layout {
    \context {
      \Voice
      \remove "Note_heads_engraver"
      \consists "Completion_heads_engraver"
    }
  }
}

