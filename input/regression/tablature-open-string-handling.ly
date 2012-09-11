\version "2.16.0"

\header {

  texidoc = "
  Open strings are part of a chord in tablature, even when @code{minimumFret} is set.
  This can be changed via @code{restrainOpenStrings}."

}

\score {
  \new TabStaff {
    \set TabStaff.minimumFret = #3
    < g, d >1
    \set TabStaff.restrainOpenStrings = ##t
    < g, d >1
  }
}
