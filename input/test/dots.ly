\score { 
  \context Voice \notes\relative c {
    \time 6/8;
	d''4. b  <b4. c d e>  <f g a b>
	<g b d> <c, g' d' a'>
	
	
	\context Staff <
		\context Voice = VA {  \stemup   f''  <b c> r4.  }
		\context Voice = VB {  \stemdown b, <a b> r4. }
	>
	
	
  }
  \paper { }  
  \midi { }
}