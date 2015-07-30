
\version "2.19.25"

\header {
  texidoc=" Other clefs for the autochanger may be set.  This works for
implicitly created staves only.
The first example should turn at b with soprano-clef in the upper Staff.
The second example should turn at d' with alto-clef in the upper and tenor-clef
in the lower Staff.
"
}

\layout { ragged-right= ##t }

music = {
  g8 b a c'
  b8 d' c'8 e'
  d'8 r f' g' a'2
}

\autochange b \with { \clef soprano } \music
\autochange d' \with { \clef alto } \with { \clef tenor } \music
