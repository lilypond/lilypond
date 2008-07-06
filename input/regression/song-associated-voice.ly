\version "2.11.51"

\include "festival.ly"

\festival #"song-associated-voice.xml" { \tempo 4 = 100 }
\relative c'
{
<< \context Voice = melody {
     \time 3/4
     c2 e4 g2.
  }
  \new Lyrics \lyricmode {
    \set associatedVoice = #"melody"
    play2 the4 game2.
  } >>
}
#(ly:progress "song-associated-voice")
#(ly:progress "~a" (ly:gulp-file "song-associated-voice.xml"))
