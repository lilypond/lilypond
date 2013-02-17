\version "2.17.11"

\header {
  texidoc = "The direction of tuplet brackets is the direction
of the majority of the stems under the bracket, with ties going
to UP.
"
}

\relative c'' {
  \tuplet 3/2 { r r r }
  \tuplet 3/2 { r c r }
  \tuplet 3/2 { r a r }
  \tuplet 3/2 { c' f,, r }
  \tuplet 3/2 { f, c'' r }
  \tuplet 3/2 { a a c }
  \tuplet 3/2 { c c a }
  \tuplet 3/2 { a a a }
  \tuplet 3/2 { c c c }
}
