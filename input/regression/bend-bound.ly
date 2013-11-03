\version "2.17.30"

\header {
  texidoc = "Bends should not be effected by the full width of a
@code{NonMusicalPaperColumn}. The bends should have identical X
spans in the two examples.
"
}

music = \repeat unfold 16 { c''4\bendAfter #-4 }
sixteens = \repeat unfold 64 { c'16 }

\new Score {
  << \music \sixteens >>
}

\new Score \with {
  currentBarNumber = #200
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #end-of-line-invisible
} {
  << \music \sixteens >>
}
