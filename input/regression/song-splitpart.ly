\version "2.11.51"

\include "festival.ly"

\score{
\festival #"song-splitpart.xml" { \tempo 4 = 100 }
<<
  \context Voice = "melody" {
    \relative c' {
      c4
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
#(ly:progress "~a" (ly:gulp-file "song-splitpart.xml"))
