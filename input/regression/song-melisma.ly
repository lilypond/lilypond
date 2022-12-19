\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
melismas.
"
}
\include "festival.ly"

\festival "song-melisma.xml" { \tempo 4 = 100 }
\relative
{
<<
  \context Voice = "lala" {
    \time 3/4
    f''4 g8
    \melisma
    f e f
    \melismaEnd
    e2
  }
  \new Lyrics \lyricsto "lala" {
    la di __ daah
  }
>>
}
#(ly:progress "song-melisma")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-melisma.xml"))
