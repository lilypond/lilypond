%{
  Currently (1.1.22.jcn5), the auto-beam engraver will only engrave
  sensible beams (hopefully), which means that it will give up the
  whole beam if:
    * a rest is encountered
    * another beam (entered manually) is encountered
    * there's a 'gap' in the beam note's durations

  There's no smart algorithm, beginning a beam is considered when
   
    now / beamAutoBegin = 0

  the beam will be ended when

    * now / beamAutoEnd = 0
%}
	
\score{
    \notes \relative c''{
    	\time 2/4;
	% one beam per measure
      	c8 c c c
	% two beams per measure
	\property Voice.beamAutoBegin = "0.25"
	\property Voice.beamAutoEnd = "0.25"
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
	    % add experimental auto-beaming
	    \consists Auto_beam_engraver;
	    % switch it on (perhaps a bit double, but we want to be able 
	    % to switch it off conveniently
	    beamAuto = 1.;
	    % consider starting beam at every 4 note
	    % would be nice if we could enter durations here, e.g.:
	    % what if we make 'dur (4)' a real_expression?
	    beamAutoBegin = 0.5;
	    beamAutoEnd = 0.5;
	}
    }
}
