\version "1.3.96";
\score { 
  \context Voice \notes\relative c'' {
    \time 6/8;
	d4. g,,
	<b''4. c d e>  <f g a b>
	<g b d> <c, g' d' a'>
	
	
	\context Staff <
		\context Voice = VA {  \stemUp   f''  <b c> r4.  }
		\context Voice = VB {  \stemDown b, <a b> r4. }
	>
	
	
  }
  \paper { }  
  \midi { }
}
