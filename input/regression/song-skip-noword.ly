\version "2.12.0"

\include "festival.ly"
#(set! *skip-word* #f)

\festival #"song-skip-noword.xml" { \tempo 4 = 100 }
\relative { c c g' }
\addlyrics {
  twin -- \skip 4
  kle
}
#(ly:progress "song-skip-noword")
#(ly:progress "~a" (ly:gulp-file "song-skip-noword.xml"))
