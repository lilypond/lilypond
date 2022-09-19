\header {

  texidoc = "Width of marks does not affect spacing."

}

\version "2.23.14"

\paper {
  ragged-right = ##t
}

\relative
{
  c''1
  \tweak self-alignment-X #CENTER \textEndMark "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
}
