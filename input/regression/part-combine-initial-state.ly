\version "2.27.5"

\header { texidoc = "Test a particular @code{\partCombine}
  configuration that does neither require a skip nor a context change
  in the first time step." }

#(ly:set-option 'warning-as-error #t)

\layout { ragged-right = ##t }

\partCombine { c''1\< \partCombineApart <>\! } { g'1\< <>\! }
