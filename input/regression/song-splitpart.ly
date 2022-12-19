\version "2.21.0"
\header {
  texidoc="Festival song synthesis output supports
divided voices.
"
}
\include "festival.ly"

\score{
\festival "song-splitpart.xml" { \tempo 4 = 100 }
<<
  \context Voice = "melody" {
    \relative {
      c'4
      <<
        { \voiceOne c8 e }
        \context Voice = splitpart { \voiceTwo c4 }
      >>
      \oneVoice c4 c | c
    }
  }
  \new Lyrics \lyricsto "melody" { we shall not o- ver- come }
  \new Lyrics \lyricsto "splitpart" { will }
>> }
#(ly:progress "song-splitpart")
#(ly:progress "~a" (ly:gulp-file-utf8 "song-splitpart.xml"))
