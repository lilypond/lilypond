\header {
texidoc = "Excercise all output functions"
  }

\relative {
  \new StaffGroup <<
    \new Staff <<
      {
      #(set-octavation 1)
      \times 2/3 {  c'8[\< c]( f''\!)  }
    }
      \skip 1 >>
    \new Staff {
      \makeClusters { <g a>8 <e a> }
      }
  >>
}
