\version "2.17.11"
\header {

  texidoc = "Tuplet numbers' outside staff priority can be
set."

}

\relative c'' {
  \override TupletBracket.avoid-scripts = ##f
  \tuplet 3/2 { a8\trill a\trill a\trill }
  \override TupletNumber.outside-staff-priority = #1
  \tuplet 3/2 { a8\trill a\trill a\trill }
  \override Script.outside-staff-priority = #2
  \tuplet 3/2 { a8\trill a\trill a\trill }
}
