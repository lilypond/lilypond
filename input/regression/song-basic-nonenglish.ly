\version "2.11.16"

\include "festival.ly"

\festivalsyl #"song-basic-nonenglish.xml" { \tempo 4 = 100 }
{
\relative { c e g r }
\addlyrics { ov -- čá -- ci }
}
#(display "song-basic-nonenglish")
#(ly:progress "~a" (ly:gulp-file "song-basic-nonenglish.xml"))
