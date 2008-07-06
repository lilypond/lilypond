\header {
  
  texidoc = "End tuplets events are sent to the starting context, so
even after a switch, a tuplet ends correctly."
  
}

\version "2.11.51"

\new Staff <<
  \partcombine
  \relative c'' {
    r2
    \times 2/3 { g8[ g g] }
    \times 2/3 { g[ g g] } g1
  }
  \relative c'' { R1 g1 }
>>

