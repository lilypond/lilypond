\version "2.15.6"
\header {

  texidoc = "Tuplet brackets' outside staff priority can be
set.  Brackets, by default, carry their numbers with them."

}

\relative c'' {
  % Plain old tuplet
  \times 2/3 { a8 r a }
  % With nothing set, collisions abound both horizontally and
  % vertically
  \times 2/3 { a8^\espressivo r a^\espressivo }
  % Setting the staff priority prevents collisions
  \override TupletBracket #'outside-staff-priority = #1
  \times 2/3 { a8^\espressivo r a^\espressivo }
  % Note that, with the outside-staff-priority set, this bracket
  % should be at the same vertical level as the first one
  \times 2/3 { a8 r a }
}
