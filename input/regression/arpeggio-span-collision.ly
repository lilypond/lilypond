\version "2.16.0"

\header {
  texidoc = "Cross-staff or -voice arpeggios which include single
note heads as anchors do not collide with previous note heads or
prefatory material."
}
ddddd = { d'16 d'16 d'16 d'16 d'4\arpeggio }
sdf = { s4 <d' f'>4\arpeggio }
\score {
  \new PianoStaff <<
    \new Staff {
      \set PianoStaff.connectArpeggios = ##t
      << \transpose c c'{ \ddddd \sdf } \\ { \sdf \ddddd } >>
      << { a'1\arpeggio } \\ { f'2\arpeggio e' } >>
    }
    \new Staff {
      R1
      d'\arpeggio
    }
  >>
  \layout {
    line-width = 90\mm
  }
}
