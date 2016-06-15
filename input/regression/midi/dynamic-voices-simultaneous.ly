\version "2.19.47"

\header {
  texidoc="The MIDI performer operates in Voice context by default,
  so dynamics in different voices are independent."
}

\score {
  \new Staff <<
    \new Voice = "A" e'2\p
    \new Voice = "A" c'2 % default dynamic expected
  >>

  \midi {}
}
