\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
slurs.
"
}
\include "festival.ly"

\festival "song-slurs.xml" { \tempo 4 = 100 }
<<
  \relative \context Voice = "lahlah" {
    \set Staff.autoBeaming = ##f
    c'4
    \slurDotted
    f8.[( g16])
    a4
  }
  \new Lyrics \lyricsto "lahlah" {
    more slow -- ly
  }
  \new Lyrics \lyricsto "lahlah" {
    \set ignoreMelismata = ##t % applies to "fas"
    go fas -- ter
    \unset ignoreMelismata
    still
  }
>>
#(ly:progress "song-slurs\n")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-slurs.xml"))
