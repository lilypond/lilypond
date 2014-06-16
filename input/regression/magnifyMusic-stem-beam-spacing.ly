\version "2.19.8"

\header {
  texidoc = "Stem lengths, beam spacing/thickness, and horizontal
spacing should be scaled along with notation size when using the
@code{\magnifyMusic} command."
}

\layout { ragged-right = ##t }

\relative <<
  { \repeat unfold 7 { g'32[ a b c] } }
  {
    \magnifyMusic 0.50 { s8_"50%" }
    \magnifyMusic 0.63 { s }
    \magnifyMusic 0.80 { s }
    \magnifyMusic 1.00 { s_"100%" }
    \magnifyMusic 1.26 { s }
    \magnifyMusic 1.59 { s }
    \magnifyMusic 2.00 { s_"200%" }
  }
>>
