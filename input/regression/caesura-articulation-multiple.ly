\version "2.23.13"

\header {
  texidoc="Articulations following @code{\\caesura} are stacked
according to the same priorities as articulations following notes.
These articulations should look the same though the input order is
different each time."
}

\fixed c' {
  e2
  \caesura \upbow \fermata \coda
  f2
  \caesura \fermata \coda \upbow

  \break

  g4
  \caesura \coda \upbow \fermata
  a4
  \caesura \coda \fermata \upbow
}
