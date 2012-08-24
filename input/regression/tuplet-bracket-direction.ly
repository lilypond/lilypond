\version "2.16.0"

\header {
  texidoc = "The direction of tuplet brackets is the direction
of the majority of the stems under the bracket, with ties going
to UP.
"
}

\relative c'' {
  \times 2/3 { r r r }
  \times 2/3 { r c r }
  \times 2/3 { r a r }
  \times 2/3 { c' f,, r }
  \times 2/3 { f, c'' r }
  \times 2/3 { a a c }
  \times 2/3 { c c a }
  \times 2/3 { a a a }
  \times 2/3 { c c c }
}
