\version "2.21.0"

\header {
  texidoc = "Clefs for cue notes and line breaks.  If the cue notes start in a
new line, the cue clef should not be printed at the end of the previous line.
Similarly, an end clef for cue notes ending at a line break should only be
printed at the end of the line.

Cue notes going over a line break should print the standard clef on the new
line plus an additional cue clef after the time/@/key signature."
}

vI = \relative { \clef "treble" \repeat unfold 40 g'4 }
\addQuote vIQuote { \vI }

Solo = \relative {
  \clef "bass"
  c1 | \break
  \cueDuringWithClef "vIQuote" #UP "tenor" { R1 } | \break
  c1 |
  \cueDuringWithClef "vIQuote" #UP "tenor" { R1 | \break
    R1 } |
  c1
}

\score {
  <<
    \new Staff \new Voice \Solo
  >>
}
