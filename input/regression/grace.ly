
\version "2.17.28"

\header{

  texidoc=" You can have beams, notes, chords, stems etc. within a
@code{\\grace} section.  If there are tuplets, the grace notes will not
be under the brace.  

Main note scripts do not end up on the grace note. 

"
}

\layout {ragged-right = ##t}

\new Voice \relative c'' {
  \grace b8 c4\fermata
  \grace {  c32 cis32 } gis4
  \grace {  cis32 dis32 } e4
  \grace {  c32 d }\tuplet 3/2 {  c8[ c c] }
  \grace {  b32 ( c32 }   c4)
  \grace  <c d>16  c8[ c8]
				%	\grace  c16  c8[ c8]
  %% broken?
  %%\grace  { \set Grace.graceAlignPosition = \right c16} c4
  c4 \grace  { c16 } c4
}


