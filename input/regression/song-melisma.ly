\version "2.16.0"
\header {
  texidoc="Festival song synthesis output supports
melismas.
"
}
\include "festival.ly"

\festival #"song-melisma.xml" { \tempo 4 = 100 }
\relative c''
{
<<
  \context Voice = "lala" {
    \time 3/4
    f4 g8
    \melisma
    f e f
    \melismaEnd
    e2
  }
  \lyricsto "lala" \new Lyrics {
    la di __ daah
  }
>>
}
#(ly:progress "song-melisma")
#(ly:progress "~a" (ly:gulp-file "song-melisma.xml"))
