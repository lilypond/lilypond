\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
repeat signs.
"
}
\include "festival.ly"

\score{
\festival "song-repetition.xml" { \tempo 4 = 100 }
<<
  \context Voice = melody \relative {
    c'2 e4 r4 | g2 e | c1 |
    \context Voice = verse \repeat volta 2 {c4 d e f | g1 | }
    a2 b | c1}
  \context Lyrics = mainlyrics \lyricsto melody  {
    do mi sol mi do
    la si do }
  \context Lyrics = mainlyrics \lyricsto verse {
   do re mi fa sol }
  \context Lyrics = repeatlyrics \lyricsto verse {
   dodo rere mimi fafa solsol }
>>
}
#(ly:progress "song-repetition")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-repetition.xml"))
