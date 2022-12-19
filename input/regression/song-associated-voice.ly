\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
associated voices.
"
}


\include "festival.ly"

\festival "song-associated-voice.xml" { \tempo 4 = 100 }
\relative c'
{
<< \context Voice = melody {
     \time 3/4
     c2 e4 g2.
  }
  \new Lyrics \lyricmode {
    \set associatedVoice = "melody"
    play2 the4 game2.
  } >>
}
#(ly:progress "song-associated-voice")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-associated-voice.xml"))
