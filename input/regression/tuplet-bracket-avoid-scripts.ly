\version "2.17.11"

\header {
  texidoc = "Tuplet brackets avoid scripts by default.
"
}

\relative c'' {
  \tuplet 3/2 { a8^\espressivo r a^\espressivo }
}
