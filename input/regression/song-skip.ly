\version "2.12.0"

\include "festival.ly"

\festival #"song-skip.xml" { \tempo 4 = 100 }
\relative { c c g' }
\addlyrics {
  twin -- \skip 4
  kle
}
#(ly:progress "song-skip")
#(ly:progress "~a" (ly:gulp-file "song-skip.xml"))
