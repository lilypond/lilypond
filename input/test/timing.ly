\score { 
  \context Voice \notes\relative c {
    
	
	% \property Score. measurePosition = #(make-moment -1 4)
	\partial 4;
	c''4 c4 c4 c2 c1
	\cadenzaOn [c8 d e f] [g a b c b c b c]
	\cadenzaOff
	c4 c4 c4 c4
	\property Score. measureLength = #(make-moment 5 4)
	
	c1 c4
	c1 c4 
	c4 c4
	\property Score. measurePosition = #(make-moment -3 8)
	b8 b b
	c4 c1
	
  }
  \paper { }  
  \midi { }
}
