\version "2.23.11"

\header {
  texidoc = "The output of @code{\\markup \\rhythm}
scales with font size automatically."
}

\markup { A syncopation: \rhythm { 16[ 8 16] } }
\markup \fontsize #0 { A syncopation: \rhythm { 16[ 8 16] } }
\markup \fontsize #-5 { A syncopation: \rhythm { 16[ 8 16] } }
