\version "2.25.35"

\header {
  texidoc = "Long spanners at the end of the lines stretch measures
correctly.
"
}

{
  \override Hairpin.minimum-length = #60
  \override Hairpin.to-barline = ##t
  \*4 a1
  a1\<
  a1\>
  a1\!
}
