\score { \notes \relative c{
c''4 c2 c8  c16 c16 c1 
\property Voice.noteHeadStyle = "diamond"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "transparent"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "cross"
c4 c2 c8  c16 c16  c1
\property Voice.noteHeadStyle = "harmonic"
c4 c2 c8  c16 c16  c1

   \context Voice <
    \context Thread = TA
      { \property Thread.noteHeadStyle = "cross"
        \property Voice.verticalDirection = \up c16} 
    \context Thread = TB
      { \property Thread.noteHeadStyle = "" a16  }
    
    \context Thread = TC
      { \property Thread.noteHeadStyle = "harmonic" d16 }
    
  >
 \context Voice <\context Thread = TA { c4 c4 }
\context Thread = TB {
  \property Thread.noteHeadStyle = "harmonic"
  c'4 \stemdown c
} >

}

    \paper {
       
    }
}
\version "1.3.5"; 
