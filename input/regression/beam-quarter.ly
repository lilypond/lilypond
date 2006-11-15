
\version "2.10.0"
\header
{
  texidoc= "Quarter notes may be beamed: the beam is halted momentarily."
}

\layout { ragged-right = ##t }

\relative c'' {
  c8[ c4 c8] % should warn here!
}
