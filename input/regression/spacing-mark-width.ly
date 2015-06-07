\header {

  texidoc = "Width of marks does not affect spacing."

}

\version "2.19.21"

\paper {
  ragged-right = ##t
}

\relative
{
  \override Score.RehearsalMark.break-visibility = #begin-of-line-invisible
  c''1
  \mark "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
}
