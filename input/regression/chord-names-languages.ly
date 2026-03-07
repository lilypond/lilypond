\version "2.25.35"

\header {
  texidoc = "
The English naming of chords (the default) can be changed to
German (@code{\\germanChords} replaces B and Bes with H and B),
semi-German (the same as @code{\\germanChords} but B gets displayed
as B with a flat), Italian (@code{\\italianChords} uses Do Re Mi
Fa Sol La Si), or French (@code{\\frenchChords} uses Do Ré Mi Fa
Sol La Si).  Command @code{\\englishChords} sets the chord names
back to English.

The input note language is the default (i.e., Dutch).
"
}

scm = \chordmode {
  e1/d c:m b/b bis/bis bes/bes beses/beses
}

\include "chord-names-languages.ily"
