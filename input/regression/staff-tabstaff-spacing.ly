\version "2.16.0"

\header {

  texidoc = "
The space between scores containing Staffs and TabStaffs should
be consistent.  In this example, all of the spacings should be
equivalent.
"
}

\score {
 { c'4 d' e' f' g'1 }
 \header {
  piece = "Title 1"
 }
}
\score {
 \new TabStaff {
  c'4 d' e' f' g'1
 }
 \header {
  piece = "Title 2"
 }
}
\score {
 { c'4 d' e' f' g'1 }
 \header {
  piece = "Title 3"
 }
}
