\version "2.27.5"

\header {
  texidoc = "Unmeetable system constraints should not cause an abort."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot find line breaking that satisfies constraints"))

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
