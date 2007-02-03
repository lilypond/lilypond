
\header
{
  texidoc = "nested properties may also be reverted. This uses
Scheme list syntax."

}

\version "2.11.15"

\relative {
  f2 \glissando c
  \override Glissando #'bound-details #'right #'Y = #4
  f2 \glissando c |
  \override Glissando #'bound-details #'left #'Y = #-4
  f2 \glissando c
  \displayMusic  \revert Glissando #'(bound-details right Y)
  f2 \glissando c
}


\paper {
  ragged-right = ##t
}
