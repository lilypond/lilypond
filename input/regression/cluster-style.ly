

\version "2.19.21"
\header {
  texidoc = "Clusters behave well across line breaks."
}

\layout { ragged-right = ##t }

fragment = \relative {
  <e' d'>4
  <g a>4 
  <e a>4
}

<<
  \time 2/4 
  \new Staff {
    \override ClusterSpanner.style = #'ramp
    \makeClusters \fragment
    r4
    \override ClusterSpanner.style = #'leftsided-stairs
    \makeClusters \fragment
    r4
    \override ClusterSpanner.style = #'rightsided-stairs
    \makeClusters \fragment
    r4
    \override ClusterSpanner.style = #'centered-stairs
    \makeClusters \fragment
    }
  
>>


