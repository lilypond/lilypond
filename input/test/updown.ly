\score { 
  \context Voice \notes\relative c {
    % this used to work:
	% <{\voiceOne e}
	% {\voiceTwo c}>
	
	% now we need:
	\context Staff
	<{\voiceOne e}
	{\voiceTwo c}>
	
	
	\version "1.3.110"; 
	
  }
  \paper { }  
  \midi { }
}
