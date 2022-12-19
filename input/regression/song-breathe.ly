\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
breath marks.
"
}
\include "festival.ly"

\festival "song-breathe.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c'2 e \breathe g }
\addlyrics { play the game }
}
#(ly:progress "song-breathe")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-breathe.xml"))
