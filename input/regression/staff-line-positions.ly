\header {

  texidoc = "The vertical positions of staff lines may be specified
  individually, by setting the @code{line-positions} property of the
  StaffSymbol."

}

\version "2.11.51"


\new Staff \relative c'  {
  \override Staff.StaffSymbol #'line-positions = #'(-7 -2 0 3 9)
  g c f b e a
}
  
  
