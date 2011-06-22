\version "2.15.1"
\header {

  texidoc = "Tuplet numbers' outside staff priority can be
set."

}

\relative c'' {
  \times 2/3 { a8\trill a\trill a\trill }
  \override TupletNumber #'outside-staff-priority = #1
  \times 2/3 { a8\trill a\trill a\trill }
  \override Script #'outside-staff-priority = #2
  \times 2/3 { a8\trill a\trill a\trill }
}
