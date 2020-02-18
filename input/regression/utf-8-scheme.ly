\version "2.21.0"

\header {
  texidoc = "In GUILE v2, embedded Scheme can contain UTF-8 strings and identifiers.
Here, identifer bääh contains music with the text \"bööh\""

}

#(define bääh #{ { c1^\markup "bööh" } #})

\new Staff \bääh
