
\version "2.6.0"

\header{

  texidoc="

There are optical corrections to the spacing of stems. The overlap between 
two adjacent stems of different direction is used as a measure for how
much to correct."

}


\context Voice \relative c {
  %% make sure neutral is down.
  \override Stem  #'neutral-direction = #-1
  \time 16/4  c''4 c c,  c' d, c' e, c' f, c' g c a c b c
}

\layout {
  raggedright = ##t
}  


