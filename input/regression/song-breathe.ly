\version "2.11.51"

\include "festival.ly"

\festival #"song-breathe.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c2 e \breathe g }
\addlyrics { play the game }
}
#(ly:progress "song-breathe")
#(ly:progress "~a" (ly:gulp-file "song-breathe.xml"))
