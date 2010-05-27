\version "2.12.0"

\header {
  texidoc="Engravers which do not exist are simply ignored"
}

\layout { \context {
  \name ImproVoice
  \type "Engraver_group"
  \consists "Rhythmic_column_engraver_foo"  % "typo" here
}
\context { \Staff
  \accepts "ImproVoice"
}}

\relative c'' {
  a4 d8 bes8 \new ImproVoice { c4^"ad lib" c
   c4 c^"undress" c_"while playing :)" c }
  a1
}