\version "2.23.2"

\header {
  texidoc = "Context-dependent orientation of the stem for a note on the middle
line of the staff can be turned off locally using the @code{suspendMelodyDecisions}
context property.

In this test, marcato marks show the expected stem direction."
}

\relative c'' {
  \time 3/4
  a8^^ b^^ g^^ f^^ b^^ g^^ |
  \set suspendMelodyDecisions = ##t
  a^^ b_^ g^^ f^^ b_^ g^^ |
  \unset suspendMelodyDecisions
  c_^ b_^ d_^ c_^ b_^ c_^ |
}

\layout {
  \context {
    \Voice
    \consists "Melody_engraver"
    \autoBeamOff
  }
}