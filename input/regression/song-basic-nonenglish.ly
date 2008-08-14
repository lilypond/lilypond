\version "2.11.51"

\include "festival.ly"

\festivalsyl #"song-basic-nonenglish.xml" { \tempo 4 = 100 }
{
\relative { c e g r }
\addlyrics { ov -- čá -- ci }
}
#(ly:progress "song-basic-nonenglish")
#(ly:progress "~a" (ly:gulp-file "song-basic-nonenglish.xml"))
