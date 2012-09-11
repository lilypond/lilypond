
\header {


  texidoc = "Percent repeats get incremental numbers when
@code{countPercentRepeats} is set, to indicate the repeat counts, but
only if there are more than two repeats."


}

\version "2.16.0"

\relative c'' \new Voice {
  \set countPercentRepeats = ##t 
  \time 4/4 
  \repeat "percent" 4 { c1 }
  \time 2/4 
  \repeat "percent" 4 { c2 c2 }
  \repeat "percent" 2 { c2 }
}

