\version "2.23.4"

\header {
  texidoc = "@code{\cadenzaOn} should not confuse the
@code{dodecaphonic-no-repeat} accidental style.
In this test, the second C should have a printed
accidental since it is not immediately repeated."
}

\new Voice \with {
  \consists Balloon_engraver
}
\relative {
  \accidentalStyle Score.dodecaphonic-no-repeat
  \cadenzaOn
  c'8 gis' gis
  \balloonGrobText NoteHead #'(1 . -2) "should have accidental"
  c,
}
