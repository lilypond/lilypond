\version "2.12.0"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "The harp-pedal markup function does some sanity checks. All 
the diagrams here violate the standard (7 pedals with divider after third), so
a warning is printed out, but they should still look okay."
}

\relative c'' {
  % Sanity checks: #pedals != 7:
  c1^\markup \harp-pedal #"^-v|--"
  % Sanity checks: no divider, multiple dividers, divider on wrong position:
  c1^\markup \harp-pedal #"^-v--v^"
  c1^\markup \harp-pedal #"^|-v|--|v^"
  c1^\markup \harp-pedal #"^-v-|-v^"
}
