%{
  Currently (1.1.27.jcn3), the auto-beam engraver will only engrave
  sensible beams, which means that it will end a beam when:
    * a rest is encountered
    * another beam (entered manually) is encountered
    * there's a 'gap' in the beam note's durations

  The beam will be ended also when

    now / beamAutoEnd = 0
%}
	
\score{
    \notes \relative c''{
    	\time 2/4;
	% one beam per measure
      	c8 c c c
      	c16 c c c c c c c
	% from here on consider ending beam every 1/4 note
	\property Voice.beamAutoEnd = "1/4"
      	c8 c c c
	% manually override autobeam with weird beaming
      	c8 [c c] c
      	c8 c c r
      	c8 c c4
      	r8 c c c
	% no autobeaming
	\property Voice.beamAuto = "0"
      	c8 c c c
    }
    \paper{
      	\translator{
	    \VoiceContext
	    % consider ending beam at every 1/2 note
	    beamAutoEnd = "1/2";
	}
    }
}
