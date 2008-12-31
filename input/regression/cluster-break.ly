
\version "2.12.0"
\header {
  texidoc = "Clusters behave well across line breaks."
}

\layout { ragged-right = ##t }

fragment = \relative c' {
  \time 2/4 
  <e d'>4
  <g a>4 | \break
  <e a>
  <f a>
}

<< \new Staff \makeClusters \fragment
   \new Staff \fragment
>>


