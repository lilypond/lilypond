\version "2.13.43"

\header {
  texidoc = "
A book(part) can contain only a label without causing a segfault.
"
}

\book {\markup "foo"}   % necessary to produce some output
\book { \label #'foo }
