\version "2.14.0"
\header {
  texidoc="Festival song synthesis output supports
skips.
"
}
\include "festival.ly"

\festival #"song-skip.xml" { \tempo 4 = 100 }
\relative c' { c c g' }
\addlyrics {
  twin -- \skip 4
  kle
}
#(ly:progress "song-skip")
#(ly:progress "~a" (ly:gulp-file "song-skip.xml"))
