%{
  wierd grouping bug
%}

\score{
    \notes \relative c''{
    	% be sure it's not related to auto-beamer
    	\property Voice.beamAuto = "0"
    	\time 1/2;
    	[ c8 c c c ]
    	\time 1/4;
    	[ c8 c ]
    }
    \paper{
    }
}
