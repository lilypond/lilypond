\score{
    \notes \relative c''{
    	\time 2/4;
	c8
	\repeat 2 {  % \bar "|:" iknoort-i ook...
	c8 c8
	}
	c8
    }
    \paper{
      	\translator{
	    \VoiceContext
	    % add experimental auto-beaming
	    \consists Auto_beam_engraver;
	    beamAuto = 1.;
	    beamAutoEnd_8 = "1/4";
	    beamAutoEnd_16 = "1/4";
	    beamAutoEnd_32 = "1/4";
	}
    }
}
