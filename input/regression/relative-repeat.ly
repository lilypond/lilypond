\header {
  texidoc = "Relative mode for repeats uses order of entry."
  }
\version "2.10.8"

\relative {
  \repeat "unfold" 3
  { f2 bes2 }
  \alternative { a1 e b } 
}
