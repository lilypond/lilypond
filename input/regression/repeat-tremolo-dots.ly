\version "2.11.51"

\header {
  texidoc = "Dots are added to tremolo notes if the durations involved require them."
}

\paper{ ragged-right = ##t }
\score { \relative c' {
        c8 \repeat "tremolo" 14 { c32 a32 } |
  }
}