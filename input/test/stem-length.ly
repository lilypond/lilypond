\version "1.5.68"

\score { 
  \context Voice \notes\relative c {
	g''4 \property Voice.Stem \set #'length = #14  g4
	\property Voice.Stem \set #'length = #3 g4  g,,4  
  }
  \paper { }  
  \midi { }
}
