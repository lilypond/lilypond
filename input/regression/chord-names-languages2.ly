\version "2.25.35"

\header {
  texidoc = "
The same as @file{chord-names-languages.ly}, but the input note
language is set to German.  The result should be exactly the
same.
"
}

\language "deutsch"

scm = \chordmode {
  e1/d c:m h/h his/his b/b heses/heses
}

\include "chord-names-languages.ily"
