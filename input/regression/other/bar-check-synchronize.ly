\version "2.25.80"

\header {
  texidoc = "Setting the deprecated context property @code{barCheckSynchronize}
triggers a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "the property 'barCheckSynchronize' is deprecated"))

\new Score \with { barCheckSynchronize = ##t } { s }
