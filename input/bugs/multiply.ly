% ugh: strange order of things, this:
%    a16 * 1/2 (
% must be 
%    a16 ( * 1/2


\score { 
  \context Voice \notes\relative c {
    % to see the magic: uncomment size stuff in init/paper20.ly
	
	c'4 c4
	
	\property Voice.fontSize= -2
	% ugh ugh
	b16 * 1 / 2 (
	\property Voice.fontSize= 0 )
	g4 *31/32
	
	a a g2
	
  }
  \paper { }  
  \midi { }
}
