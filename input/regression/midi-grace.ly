
\header {

  texidoc = "Grace notes don't intrroduce syncing problems: the last note
off will appear at tick 768 (2 * 384)."
  
}
\version "2.19.21"
\score {
 \relative {
   c'4
   \grace { b8 }
   d4
 }
 \midi { }
}
