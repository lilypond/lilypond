\version "2.21.0"

\header {
  texidoc="
Number of frets and number of strings can be changed from the
defaults.

"
}

\layout { ragged-right = ##t }

<<
  \chords {
    d1 |
    a1
  }

  \new Voice {
    \textLengthOn

    % Simple Guitar D diagram with defaults

    d'1 ^\markup {
      \fret-diagram "6-x;5-x;4-o;3-2-1;2-3-2;1-2-3;"
    } |

    % A chord for ukulele -- change both string and fret count
    a'1 ^\markup {
      \fret-diagram "w:4;h:5;4-2-2;3-1-1;2-o;1-o;"
    }
  }
>>


