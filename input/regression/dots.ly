#(ly:set-option 'old-relative)
\version "1.9.4"
\header{
texidoc="
Noteheads can have dots, and rests can too.  Augmentation dots should
never be printed on a staff line, but rather be shifted vertically. They
should go up, but in case of multiple parts, the down stems have down
shifted dots.  (Wanske p. 186) In case of chords, all dots should be in
a column.  The dots go along as rests are shifted to avoid collisions.
"
}



\score { 
  \context Voice \notes\relative c'' {
    \time 6/8
	d4. g,,
	<b'' c d e>4.  <f g a b>
	<g b d> <c, g' d' a'>
	
	
	\context Staff <<
	     { f''  <b c> r4.  }\\
	     { b, <a b> r4. }
	>>
	
	
  }
  \paper { }  
  \midi { }
}

