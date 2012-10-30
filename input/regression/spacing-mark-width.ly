\header {

  texidoc = "Width of marks does not affect spacing."

}

\version "2.17.6"

\paper {
  ragged-right = ##t
}

\relative c''
{
  \override Score.RehearsalMark.break-visibility = #begin-of-line-invisible
  c1
  \mark "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
}
