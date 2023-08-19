\version "2.24.3"

\header {
  texidoc = "
@code{Duration_line_engraver} works nicely with @code{\\partCombine}.

If @code{\\partCombine} combines notes to chords both note heads get a
@code{DurationLine}."
}

\layout {
  \context {
    \Voice
    \consists "Duration_line_engraver"
  }
}

\partCombine
  \relative c' { e2\- r e' r }
  \relative c' { c2 r c\- r }
