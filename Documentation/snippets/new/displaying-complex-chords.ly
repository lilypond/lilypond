\version "2.13.1"

\header {
  lsrtags = "simultaneous-notes, chords"
  texidoc = "
Here is a way to display a chord where the same note is played twice
with different accidentals.
"
  doctitle = "Displaying complex chords"
}

fixA = {
  \once \override Stem #'length = #9
}
fixB = {
  \once \override NoteHead #'X-offset = #1.7
  \once \override Stem #'rotation = #'(45 0 0)
  \once \override Stem #'extra-offset = #'(-0.2 . -0.2)
  \once \override Stem #'flag-style = #'no-flag
  \once \override Accidental #'extra-offset = #'(4 . 0)
}

\relative c' {
  << { \fixA <b d!>8 } \\ { \voiceThree \fixB dis } >> s
}
