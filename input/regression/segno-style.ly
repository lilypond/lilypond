\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Segni are printed as marks or bar lines according to the
@code{segnoStyle} context property.  The @code{mark} style, which is
the default, yields marks only.  When the style is set to
@code{bar-line}, the default @code{segnoMarkFormatter} skips the mark
for segno 1, but allows marks on later segni to eliminate ambiguity.
The user can override the segno formatter with a rehearsal-mark
formatter.  Rehearsal marks and segni are sequenced independently."
}

piece = \fixed c' {
  \time 3/4 % for comparison of numbers
  %% A segno is not normally printed at the beginning of the piece;
  %% specifying a number forces it to appear.
  \segnoMark 1
  %% This test is intended to be sensitive to enhancements that would
  %% allow different behavior where bar lines are not measure-aligned.
  r2
  %% Ask for #1 again to make sure that the bar-line style doesn't
  %% merely drop the first segno mark of the piece, but always #1.
  \segnoMark 1 % alone
  r4 | \mark \default r2
  \segnoMark \default % at start-repeat bar
  \repeat volta 2 { r4 | r2 }
  \segnoMark \default % at double-sided repeat bar
  \repeat volta 2 { r4 | \mark \default r2 }
  \segnoMark 96 % at end-repeat bar
  r4
}

\new Score {
  \new Staff \with { instrumentName = "default" } {
    \piece
  }
}

\new Score \with {
  instrumentName = "'bar-line"
  segnoStyle = #'bar-line
} {
  \new Staff \with { instrumentName = "bar-line" } {
    \piece
  }
}

\new Score \with {
  segnoStyle = #'bar-line
  segnoMarkFormatter = #format-mark-box-numbers
} {
  \new Staff \with {
    instrumentName = \markup \column { "bar-line &" "formatter" }
  } {
    \piece
  }
}
