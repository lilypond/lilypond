\version "2.21.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Harp pedal diagram contains dividers at positions ~a.  Normally, there is only one divider after the third pedal.") '(1 3 5))
#(ly:expect-warning (G_ "Harp pedal diagram contains dividers at positions ~a.  Normally, there is only one divider after the third pedal.") '(4))
#(ly:expect-warning (G_ "Harp pedal diagram contains ~a pedals rather than the usual 7.") 5)
#(ly:expect-warning (G_ "Harp pedal diagram does not contain a divider (usually after third pedal)."))


\header {
  texidoc = "The harp-pedal markup function does some sanity checks. All 
the diagrams here violate the standard (7 pedals with divider after third), so
a warning is printed out, but they should still look okay."
}

\relative {
  \override Score.PaperColumn.keep-inside-line = ##f
  % Sanity checks: #pedals != 7:
  c''1^\markup \harp-pedal "^-v|--"
  % Sanity checks: no divider, multiple dividers, divider on wrong position:
  c1^\markup \harp-pedal "^-v--v^"
  c1^\markup \harp-pedal "^|-v|--|v^"
  c1^\markup \harp-pedal "^-v-|-v^"
}
