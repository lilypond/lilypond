\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
basic songs.
"
}
\include "festival.ly"

\festival "song-basic.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c'2 r2 e4 g2. }
\addlyrics { play the game }
}
#(ly:progress "song-basic")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-basic.xml"))
