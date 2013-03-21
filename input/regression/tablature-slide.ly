\header
{

  texidoc = "Tab supports slides."
}

\version "2.17.15"
\paper {
  ragged-right = ##T
}


\relative c' \new TabVoice
{
  <c g'\harmonic> d\2\glissando e\2
}
