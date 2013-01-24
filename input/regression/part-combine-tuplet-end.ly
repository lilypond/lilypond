\header {
  
  texidoc = "End tuplets events are sent to the starting context, so
even after a switch, a tuplet ends correctly."
  
}

\version "2.17.11"

\new Staff <<
  \partcombine
  \relative c'' {
    r2
    \tuplet 3/2 { g8[ g g] }
    \tuplet 3/2 { g[ g g] } g1
  }
  \relative c'' { R1 g1 }
>>

