\version "2.24.1"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This tests @code{Staff.midiPanPosition}.  The sound
should begin with an oboe at left and a bassoon at right.  They should
switch positions in three steps at intervals of two seconds."
}

\score {
  \midi {}
  <<
    \tempo 4 = 30
    \new Staff \with {
      midiInstrument = "oboe" % something sustained for audio verification
      midiPanPosition = -1.0
    } <<
      a'1
      {
        \skip 4
        \set Staff.midiPanPosition = -0.33
        \skip 4
        \set Staff.midiPanPosition = 0.33
        \skip 4
        \set Staff.midiPanPosition = 1.0
      }
    >>
    \new Staff \with {
      midiInstrument = "bassoon" % something sustained for audio verification
      midiPanPosition = 1.0
    } <<
      f,1
      {
        \skip 4
        \set Staff.midiPanPosition = 0.33
        \skip 4
        \set Staff.midiPanPosition = -0.33
        \skip 4
        \set Staff.midiPanPosition = -1.0
      }
    >>
  >>
}
