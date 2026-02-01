\version "2.25.33"

ddddd = { d'16 d'16 d'16 d'16 d'4\testCommand }
sdf = { s4 <d' f'>4\testCommand }
\score {
  \new PianoStaff <<
    \new Staff {
      \set #(list 'PianoStaff testConnectPropertyName) = ##t
      << \transpose c c' { \ddddd \sdf } \\ { \sdf \ddddd } >>
      << { a'1\testCommand } \\ { f'2\testCommand e' } >>
    }
    \new Staff {
      R1
      d'\testCommand
    }
  >>
  \layout {
    line-width = 90\mm
  }
}
