\version "2.16.0"
\paper { ragged-right = ##t }

\header {
  texidoc = "When a tie is broken, the spacing engine must consider the
accidental after the line break.  The second and third lines should have
the same note spacing."
}

{ \key bes \major r1 \break
  eses''4 r2 eses''4~ \break
  eses''4 r2 f''4
}


