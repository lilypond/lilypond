\header {
  texidoc = "Excercise all output functions"
}

\version "2.5.0"

\relative {
  \new StaffGroup <<
    \new Staff <<
      {
	#(set-octavation 1)
	\times 2/3 {  c'8[\< f]( f''\!)  }
	#(set-octavation 0)
	<f \5>
      }
      \skip 1 >>
    \new Staff \relative c'' {
      \makeClusters { <g a>8 <e a> }
    }
  >>
}
