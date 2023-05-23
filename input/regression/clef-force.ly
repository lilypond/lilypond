\version "2.24.1"

\header {
  texidoc = "Setting @code{forceClef} prints a clef, even at the end of the
music, even if the clef is unchanged, and even without a @code{\\clef} command."
}

{
  c'1
  \once \set Staff.forceClef = ##t
}
