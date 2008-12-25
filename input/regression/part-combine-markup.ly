\version "2.12.0"

\header {
  texidoc = "Part combine texts accept markup."
}

\new Staff <<
  \set Score.soloText = \markup { \concat { I \super o } }
  \set Score.soloIIText = \markup { \huge \italic II }
  \set Score.aDueText = \markup { \normal-text \rounded-box { "a 2" } }
  \partcombine
    \relative g' { g4 g r r a2 g }
    \relative g' { r4 r a( b) a2 g }
>>
