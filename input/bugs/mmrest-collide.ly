\version "1.5.68"
\header {
texidoc="multi-measure-rests (bar 2) should `collide' (ie, not clash)
 with notes, just as normal rests (bar 1)."
}

\score {
  \notes\context Staff <
     \context Voice=one \relative c''{
       d d d d
       d d d d
     } 
     \context Voice=two {
       r1
       R
     } 
  >
}