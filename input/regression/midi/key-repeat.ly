\version "2.27.5"
\header {
  texidoc = "A key signature repeat is ignored in MIDI output."
}

\score {
  \relative {
    \key f \major
    f'1
    \key \default
    f'1
  }
  \midi { }
}
