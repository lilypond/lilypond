
\version "2.7.13"
\header{
  texidoc = "Here @code{startGraceMusic} should set @code{no-stem-extend} to 
true; the two grace beams should be the same here.
"
}


\layout {
  raggedright = ##t
}  

\context Voice \relative c {
  \grace { 
    f8[ e8] 
    \override Stem  #'no-stem-extend = ##t
    f8[ e8] 
    \revert Stem #'no-stem-extend
  }
  a4
  
}

