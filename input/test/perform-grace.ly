
\score { 
  \context Voice \notes\relative c {
    \context Voice=VoiceOne
	\property Voice.graceFraction = #(make-moment 1 4) 
	\grace c8 d4 d d d d
	\property Voice.graceFraction = #(make-moment 1 2)
	\grace { e16 f e f } d4 d d d d 
	
  }
  \paper { }  
  \midi { }
}
