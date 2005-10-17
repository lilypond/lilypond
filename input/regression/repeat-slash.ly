\version "2.7.13"
\header {
  texidoc = "Within a bar, beat repeats denote that a music snippet should be
played again."
}

\layout { raggedright = ##t }

\relative c'
\context Voice {
  \time 4/4
  \repeat "percent" 2 { c2 }

  %% the chairman dances
  \repeat "percent" 2 { g'8 g c, c  }   
  \repeat "percent" 4 { b8 b8  }
}


