\version "2.19.11"

\header {
  texidoc = "All text-interface grobs that are within the Staff
context should have @w{@code{baseline-skip}} and
@w{@code{word-space}} values scaled along with notation size when
using the @code{\magnifyStaff} command."
}

\paper {
  line-width = 60
  ragged-right = ##f
  score-system-spacing = #'((padding . 3))
}

\layout {
  \context {
    \Staff
    \override TextScript.baseline-skip = 2
    \override InstrumentName.baseline-skip = 2
    \override InstrumentName.self-alignment-X = 0.75
  }
}

abcd = \markup {
  \center-column { a c }
  \center-column { b d }
}

music = {
  \set Staff.instrumentName = \markup \abcd
  b'1^\markup \abcd
}

\new Staff { \magnifyStaff 0.5 \music }
\new Staff { \magnifyStaff 1.0 \music }
\new Staff { \magnifyStaff 2.0 \music }
