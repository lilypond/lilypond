\header {

texidoc = "Automatic fingering tries to put fingering instructions
next to noteheads.  scriptHorizontal puts the center fingerings horizontally next to
the note heads.

For this to function, you have to @code{\apply} pitchify-scripts to
the music you're dealing with, and you have to do so outside of a
@code{\relative} block.  "

}

\score {

 \notes\relative c' {
 c4-4
  <c-1 f-4>
  < c-1 e-2 g-3  b-4 >
\apply #pitchify-scripts \relative c'  { c4-4
  <c-1 f-4>
 < c-1 e-2 g-3  b-4 >
 \property Voice.scriptHorizontal = ##t
   <c-1 f-4>
   <c-1 f-4 a-5>   
 < c-1 e-2 g-3  b-4 >   
 } } }
