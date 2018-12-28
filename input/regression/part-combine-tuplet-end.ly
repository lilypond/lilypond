\header {
  
  texidoc = "End tuplets events are sent to the starting context, so
even after a switch, a tuplet ends correctly."
  
}

\version "2.21.0"

\new Staff <<
  \partCombine
  \relative {
    r2
    \tuplet 3/2 { g'8[ g g] }
    \tuplet 3/2 { g[ g g] } g1
  }
  \relative { R1 g'1 }
>>

