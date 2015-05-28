\version "2.19.21"
\header {

  texidoc = "Tuplet brackets' outside staff priority can be
set.  Brackets, by default, carry their numbers with them."

}

\relative {
  \override TupletBracket.avoid-scripts = ##f
  % Plain old tuplet
  \tuplet 3/2 { a'8 r a }
  % With nothing set, collisions abound both horizontally and
  % vertically
  \tuplet 3/2 { a8^\espressivo r a^\espressivo }
  % Setting the staff priority prevents collisions
  \override TupletBracket.outside-staff-priority = #1
  \tuplet 3/2 { a8^\espressivo r a^\espressivo }
  % Note that, with the outside-staff-priority set, this bracket
  % should be at the same vertical level as the first one
  \tuplet 3/2 { a8 r a }
}
