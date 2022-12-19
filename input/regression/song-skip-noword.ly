\version "2.21.0"
\header {
  texidoc = "Festival song synthesis output supports
lyrics which are not complete words.
"
}
\include "festival.ly"
#(*skip-word* #f)

\festival "song-skip-noword.xml" { \tempo 4 = 100 }
\relative { c'4 c g' }
\addlyrics {
  twin -- \skip 4
  kle
}

#(ly:progress "song-skip-noword")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-skip-noword.xml"))

#(*skip-word* "-skip-")
