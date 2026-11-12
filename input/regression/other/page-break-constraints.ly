\version "2.27.6"

\header {
  texidoc = "Unmeetable system constraints should not cause an abort."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme
		     "cannot fit the music into %d systems; using %d")
  2 3)

\layout {
  system-count = #2
}

{
  c1
  \pageBreak
  c1
  \pageBreak
  c1
}
