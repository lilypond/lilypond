
\version "2.7.13"

\header{

  texidoc="

There are optical corrections to the spacing of stems. The overlap between 
two adjacent stems of different direction is used as a measure for how
much to correct."

}

\layout {
  raggedright = ##t
}


\context Voice \relative c {
  %% make sure neutral is down.
  \override Stem  #'neutral-direction = #down
  \time 16/4  c''4 c c,  c' d, c' e, c' f, c' g c a c b c
}

  


