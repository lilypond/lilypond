
\header {

  texidoc = "Grace notes don't intrroduce syncing problems: the last note
off will appear at tick 768 (2 * 384)."
  
}
\version "2.11.51"
\score {
 \relative c' {
   c4
   \grace { b8 }
   d4
 }
 \midi { }
}
