\score { 
  \context Voice \notes\relative c {
    % this used to work (long time ago):
	% <{\voiceOne e}
	% {\voiceTwo c}>
	
	% now we need:
	<\context Voice=one {\voiceOne e}
	\context Voice=two {\voiceTwo c}>
	
  }
  \paper { }  
  \midi { }
}
