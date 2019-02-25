\version "2.21.0"

\header {
  texidoc="
The size, spacing, and symbols used to indicate open and muted strings
can be changed.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    d1 |
    d1 |
    d1
  }

  \new Voice {
    \textLengthOn

     %% D major for guitar, terse style
    d'1 ^\markup {
            \fret-diagram-terse "x;x;o;2-1;3-2;2-3;"}

     %% D major for guitar, terse style
    \once \override TextScript.fret-diagram-details.top-fret-thickness = #5
    \once \override TextScript.fret-diagram-details.xo-font-magnification = #0.3
    d'1 ^\markup {
            \fret-diagram-terse "x;x;o;2-1;3-2;2-3;"}

     %% D major for guitar, terse style
    \override TextScript.fret-diagram-details.mute-string = "M"
    \override TextScript.fret-diagram-details.open-string = "*"
    \override TextScript.fret-diagram-details.xo-padding = #0.5
    d'1 ^\markup {
            \fret-diagram-terse "x;x;o;2-1;3-2;2-3;"}

 }
>>


