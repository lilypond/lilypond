\version "2.11.51"

\include "festival.ly"

\festival #"song-reordering.xml" { \tempo 4 = 100 }
<<
  \relative \context Voice = "lahlah" {
    \set Staff.autoBeaming = ##f
    c4
    <<
      \context Voice = alternative {
        \voiceOne
        \times 2/3 {
          \override NoteColumn #'force-hshift = #-3
          f8 f g
        }
      }
      {
        \voiceTwo
        f8.[ g16]
        \oneVoice
      } >>
    a8( b) c
  }
  \new Lyrics \lyricsto "lahlah" {
    Ju -- ras -- sic Park
  }
  \new Lyrics \lyricsto "lahlah" {
    \set associatedVoice = alternative % applies to "ran"
    Ty --
    ran --
    no --
    \set associatedVoice = lahlah % applies to "rus"
    sau -- rus Rex
  } >>
#(ly:progress "song-reordering")
#(ly:progress "~a" (ly:gulp-file "song-reordering.xml"))
