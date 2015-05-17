
\version "2.19.21"
\header {
  texidoc = "Clusters are a device to denote that a complete range of
notes is to be played."
}
  \layout { ragged-right = ##t }


fragment = \relative {
  c'4 f4 <e d'>4
  <g a>8 <e a> a4 c2 <d b>4 e4 
  c4
}

<<
  \new Staff \fragment
  \new Staff \makeClusters \fragment
>>




