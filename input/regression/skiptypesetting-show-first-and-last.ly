\header {

  texidoc = "@code{showFirstLength} and @code{showLastLength} may be set at the
  same time; both the beginning and the end of the score will be printed."

  }

\version "2.12.0"

showFirstLength = R1*1
showLastLength = R1*2
\paper {
  ragged-right = ##T
}

{
  c1 d e f g a
}
