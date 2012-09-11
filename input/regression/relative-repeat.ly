\header {
  texidoc = "Relative mode for repeats uses order of entry."
  }
\version "2.16.0"

\relative c' {
  \repeat "unfold" 3
  { f2 bes2 }
  \alternative { a1 e b } 
}
