
\header {

  texidoc = "Show tuplet numbers also on single-note tuplets (otherwise the timing would look messed up!), but don't show a bracket. Make sure that tuplets without any notes don't show any number, either."

}
\version "2.11.51"

\paper { ragged-right = ##t
indent = 0.0 }

\new Staff <<
  \new Voice \relative c'' {
    \times 4/6 { c2.:8 } \times 2/3 { g4.:8 } \times 2/3 { a,4.:8 } \times 4/6 {} \bar"|."
  }
>>
  
