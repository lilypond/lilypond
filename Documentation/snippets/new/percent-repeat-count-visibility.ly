\version "2.25.35"

\header {
  categories = "Repeats, Tweaks and overrides"

  texidoc = "
Percent repeat counters can be shown at regular intervals by setting
the context property @code{repeatCountVisibility}.
"

  doctitle = "Percent repeat count visibility"
} % begin verbatim


\relative c'' {
  \set countPercentRepeats = ##t
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 5)
  \%10 c1 \break
  \set repeatCountVisibility = #(every-nth-repeat-count-visible 2)
  \%6 { c1 d1 }
}
