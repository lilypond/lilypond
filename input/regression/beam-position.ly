
\version "2.19.21"
\header{

  texidoc=" Beams on ledgered notes should always reach the middle staff
line.  The second beam, counting from the note head side, should never
be lower than the second staff line.  This does not hold for grace
note beams.  Override with @code{no-stem-extend}.  "

}

\layout {
  ragged-right = ##t
}  


\context Voice \relative {
  f8[ f]   f64[ f] 
  \grace { 
    f8[ e8] 
    \override Stem.no-stem-extend = ##f
    f8[ e8] 
    \revert Stem.no-stem-extend
  }
  f8[ f]
  
}
