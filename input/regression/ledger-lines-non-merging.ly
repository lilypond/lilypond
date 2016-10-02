\version "2.19.49"

\header {
  texidoc = "In some rare cases like these the
extents of two ledger lines at the same vertical
position in the same note column do not overlap
horizontally, and they should not be merged into
a single ledger line.
See LSR 505: Displaying complex chords
http://lsr.di.unimi.it/LSR/Item?id=505
"
}

fixA = {
  \once \override Stem.length = #11
}

fixB = {
  \once \override NoteHead.X-offset = #1.7
  \once \override Stem.length = #7
  \once \override Stem.rotation = #'(45 0 0)
  \once \override Stem.extra-offset = #'(-0.1 . -0.2)
  \once \override Flag.style = #'no-flag
  \once \override Accidental.extra-offset = #'(4 . -.1)
}

\relative c' {
  % case 1
  \pitchedTrill a'' \startTrillSpan a
  % case 2
  << { \fixA <b,,, d!>8 } \\ { \voiceThree \fixB dis } >> s
}
