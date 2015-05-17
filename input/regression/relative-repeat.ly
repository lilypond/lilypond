\header {
  texidoc = "Using repeat unfold within a relative block gives a
different result from writing the notes out in full.  The first
system has all the notes within the stave.  In the second, the
notes get progressively higher."
  }
\version "2.19.21"

\relative {
  c''1^"Using unfold"
  \repeat unfold 3 { f,2^"Repeated" bes2 }
  \alternative { { a2_"Alt1" c } { e_"Alt2" c } { b_"Alt3" d } }
}

\relative {
  c''1^"The same notes, written out"
  f,2 bes2 a2 c f2 bes2 e c f2 bes2 b d
}
