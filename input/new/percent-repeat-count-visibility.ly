\version "2.12.0"
\header {
  lsrtags = "repeats, tweaks-and-overrides"
  texidoc = "
Percent repeat counters can be shown at regular intervals by setting
the context property @code{repeatCountVisibility}.
"
  doctitle = "Percent repeat count visibility"
}

\relative c'' {
  \set countPercentRepeats = ##t
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 5)
  \repeat percent 10 { c1 } \break
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 2)
  \repeat percent 6 { c1 d1 }
}
