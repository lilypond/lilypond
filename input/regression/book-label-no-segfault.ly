\version "2.13.43"

\header {
  texidoc = "
A book(part) can contain only a label without causing a segfault.
"
}

\book { \label #'foo }
