\version "2.11.51"

\include "festival.ly"

\score{
\festival #"song-repetition.xml" { \tempo 4 = 100 }
<<
  \context Voice = melody \relative c' {
    c2 e4 r4 | g2 e | c1 |
    \context Voice = verse \repeat volta 2 {c4 d e f | g1 | }
    a2 b | c1}
  \lyricsto melody  \context Lyrics = mainlyrics \lyricmode {
    do mi sol mi do
    la si do }
  \lyricsto verse \context Lyrics = mainlyrics \lyricmode {
   do re mi fa sol }
  \lyricsto verse \context Lyrics = repeatlyrics \lyricmode {
   dodo rere mimi fafa solsol }
>>
}
#(ly:progress "song-repetition")
#(ly:progress "~a" (ly:gulp-file "song-repetition.xml"))
