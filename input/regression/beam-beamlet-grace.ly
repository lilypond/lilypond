\header
{
  texidoc = "Beamlets in grace notes remain readable."
}

\version "2.19.21"
\layout {
  ragged-right = ##t
}

\relative
\context Staff {
  f'1 \grace { a'8 f16 } g1
}
