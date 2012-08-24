\version "2.16.0"
\header {
  texidoc="Festival song synthesis output supports
non-english syllabels.
"
}
\include "festival.ly"

\festivalsyl #"song-basic-nonenglish.xml" { \tempo 4 = 100 }
{
\relative c' { c e g r }
\addlyrics { ov -- čá -- ci }
}
#(ly:progress "song-basic-nonenglish")
#(ly:progress "~a" (ly:gulp-file "song-basic-nonenglish.xml"))
