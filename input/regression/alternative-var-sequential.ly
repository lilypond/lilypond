\version "2.25.26"

\header {
  texidoc="@code{\\alternative} accepts sequential music in a variable.  The
staff labels show the repeat structure that is expected when the input is
correctly parsed."
}

#(ly:set-option 'warning-as-error #t)

alts = { s1_"B" s1_"C" }

\new Staff \with { instrumentName = "ABAC" } {
  \repeat volta 2 { s1_"A" \alternative \alts }
}

\new Staff \with { instrumentName = "ABAC" } {
  \repeat volta 2 s1_"A" \alternative \alts
}
