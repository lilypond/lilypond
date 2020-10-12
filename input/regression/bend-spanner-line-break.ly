\version "2.23.0"

\header {
  texidoc = "At a line break the @code{BendSpanner} avoids changed
@code{TimeSignature}, @code{KeySignature}, @code{KeyCancellation} and
@code{Clef} in other staves."
}

line-break-spacing = {
  d1\^ e
}

control = {
  \key f \major
  s1
  \clef "alto" \time 2/2 \key cis \major \break
  s1
  \bar "|."
}


\score {
  \new StaffGroup
    <<
      \new Staff << { \clef "G_8" \line-break-spacing } \control >>
      \new TabVoice \line-break-spacing
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
    \context {
      \TabStaff
      minimumFret = #3
      restrainOpenStrings = ##t
    }
  }
}
