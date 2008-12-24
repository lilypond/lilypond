\version "2.12.0"

\include "festival.ly"

\festival #"song-basic.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c2 r2 e4 g2. }
\addlyrics { play the game }
}
#(ly:progress "song-basic")
#(ly:progress "~a" (ly:gulp-file "song-basic.xml"))
