\version "2.11.51"
\header{

  texidoc="For longer measure lengths, the breve rest is used."

}

\layout {
  \context {
    \Score
    skipBars = ##t
  }
  ragged-right = ##t	
}


{
  \time 8/4
  R1*12
  \time 4/4
  R1*6
}


