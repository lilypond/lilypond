\version "2.25.6"

\header {
  texidoc = "A tuplet with only skips is silently omitted."
}

#(ly:set-option 'warning-as-error)

{
  \time 2/4
  \tuplet 3/2 { s4 s4 s4 }
  \set tupletFullLength = ##t
  \tuplet 3/2 { s4 s4 s4 }
  \set tupletFullLengthNote = ##t
  \tuplet 3/2 { s4 s4 s4 }
}
