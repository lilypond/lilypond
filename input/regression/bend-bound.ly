\version "2.17.30"

\header {
  texidoc = "Bends should not be affected by the full width of a
@code{NonMusicalPaperColumn}.  The bends should have identical X
spans in the two scores.  No bends should cross bar lines.
"
}

music = \fixed c'' {
  c4\bendAfter #-4
  c4\bendAfter #-4
  \repeat volta 2 {
    c2\bendAfter #-4
    |
    c2\bendAfter #-4
  }
  c4\bendAfter #-4
  c4\bendAfter #-4
  |
  c4\bendAfter #-4
  c4\bendAfter #-4
  \bar "||"
  c2\bendAfter #-4
  |
  c2\bendAfter #-4
  \bar "||"
  c4\bendAfter #-4
  c4\bendAfter #-4
  |
  \bar "|."
}

sixteenths = \repeat unfold 64 { c'16 }

\new Score {
  << \music \sixteenths >>
}

\new Score \with {
  currentBarNumber = #200
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #end-of-line-invisible
} {
  << \music \sixteenths >>
}
