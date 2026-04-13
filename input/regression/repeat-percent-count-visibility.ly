\version "2.25.35"

\header {
  texidoc = "
Percent repeat counters can be shown at regular
intervals by setting @code{repeatCountVisibility}.
"
}

\relative {
  \set countPercentRepeats = ##t
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 5)
  \%10 c''1 \break
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 2)
  \%6 { c1 d1 }
}
