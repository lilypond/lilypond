\version "2.25.0"

\header {
  texidoc = "Tuplet numbers that are slightly outside the staff
sit on the staff line."
}

#(set-global-staff-size 10)

\relative f' {
  \time 5/4
  \tuplet 3/2 { f8 f f }
  \tuplet 3/2 { e e e }
  \tuplet 3/2 { d d d }
  \tuplet 3/2 { c c c }
  \tuplet 3/2 { b b b } 
  \override TupletBracket.bracket-visibility = ##t
  \tuplet 3/2 { f' f f }
  \tuplet 3/2 { e8 e e }
  \tuplet 3/2 { d d d }
  \tuplet 3/2 { c c c }
  \tuplet 3/2 { b b b }
}
