\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
multiple stanzas.
"
}
\include "festival.ly"

\festival "song-stanzas.xml" { \tempo 4 = 100 }
{
\time 3/4
\relative { c'2 e4 g2. }
\addlyrics { play the game }
\addlyrics { speel het spel }
\addlyrics { joue le jeu }
}
#(ly:progress "song-stanzas")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-stanzas.xml"))
