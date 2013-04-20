\version "2.17.16"

\header {
  texidoc = "Long spanners at the end of the lines stretch measures
correctly.
"
}

{
  \override Hairpin.minimum-length = #60
  \override Hairpin.to-barline = ##t
  \repeat unfold 4 a1
  a1\<
  a1\>
  a1\!
}
