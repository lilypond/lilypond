
\version "2.3.1"
\header {
    texidoc = "Clusters are a device to denote that a complete range of
notes is to be played."
}

fragment =\notes \relative c' {
         c4 f4 <e d'>4
         <g a>8 <e a> a4 c2 <d b>4 e4 
         c4 }

\score {
 \notes <<
     \new Staff \fragment
     \new Staff \applymusic #notes-to-clusters \fragment
     >>
 \paper { raggedright = ##t }
}



