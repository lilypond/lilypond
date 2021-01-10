\version "2.23.0"

\header  {
  texidoc = "Scripts and texts may be added to the multi-measure
rests.  This test covers such rests under various spanners.  This used
to crash (issue #6085)."
}

\new Score {
  \tuplet 3/2 { f'1*3/2 R1.\shortfermata e''1*3/2 }
}

\new Score {
  f'1\( R1\shortfermata e''1\)
}

\new Score {
  \repeat volta 2 {} \alternative { { f'1 R1\shortfermata e''1 } }
}
