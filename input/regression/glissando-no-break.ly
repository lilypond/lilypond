\version "2.23.10"

\header {

  texidoc = "Glissandi are not broken. Output of this test
is expected to run off the page."
}

\layout {
  ragged-right = ##t
  line-width = 10\cm
}

{
  \repeat unfold 30 { c''2 c'2\glissando }
  c'2
}
