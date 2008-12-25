\header {

  texidoc = "Generate valid postscript even if dash-period is small
  compared to line thickness."

}

\version "2.12.0"

\relative {
  \override Staff.OttavaBracket #'dash-period = #0.1
  \override Score.OttavaBracket #'dash-fraction = #1

  \ottava #1
  c4 c
}

