\header
{
  texidoc = "Beamlets in grace notes remain readable."
}

\version "2.11.51"
\layout {
  ragged-right = ##t
}

\relative c'
\context Staff {
  f1 \grace { a'8[ f16] } g1
}
