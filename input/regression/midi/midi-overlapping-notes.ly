\header {

  texidoc = "Notes with equal pitch overlapping in time on the same
MIDI channel should produce multiple MIDI note-on events, but no
isolated note-off events before the last of these notes ends."

}

\version "2.19.45"

\score {
  \new Staff \with { midiInstrument = "church organ" }
  \new Voice \relative c' <<
    % This combination of music expressions should not produce audible
    % pauses between notes in the MIDI output, in particular, before the
    % 'g1' note in the first expression.
    { c1 s1 g1 }
    { s4 c4 }
    { s2 c2~ c1 s1 }
  >>
  \midi { }
}
