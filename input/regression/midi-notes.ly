\header {

  texidoc = "A MIDI note-off event precedes a simultaneous note-on event
for the same pitch in the same MIDI channel, so that all notes are heard.
Run @code{timidity -idvvv file.midi |grep Midi} to see midi events."

}

\version "2.16.0"

\score {
  <<
    \new Staff <<
      {r4 g' r2 | r2 a'4 r } \\
      {g'4 r r2 | r2 r4 a' }
    >>
    \new Staff { r2 g'2 | a'2 r2 }
  >>
  \midi {}
  \layout {}
}
