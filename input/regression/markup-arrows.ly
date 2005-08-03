
\header {
  texidoc = "The feta font has arrow heads"
}


\lyrics {
  \markup {

    filled
    
    \arrow-head #0 #1 ##t
    \arrow-head #0 #-1 ##t
    \arrow-head #1 #1 ##t
    \arrow-head #1 #-1 ##t

    
    \arrow-head #0 #1 ##f
    \arrow-head #0 #-1 ##f
    \arrow-head #1 #1 ##f
    \arrow-head #1 #-1 ##f
  }
}

\version "2.7.4"
