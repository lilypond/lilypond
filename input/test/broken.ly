\version "1.7.18"
% TODO: move to regression or remove?

\header{
texidoc = 	 "This file tests  slurs"
}

%% todo: this is very old, still talks about "feta embedded" slurs? 


shortlong =  \notes{
	c4(c-)( c c  |
	c c c c |
	c c c c |
	\break
	c c c c-) |
}

broken =  \notes\transpose c c'{

      c c c c(
      c-) c c c(
      c c-) c c(
      c c c-) c(
      a'-) a' a' a'()
      a' a' a' a'()
      c( c c c-) 
      c( c c f,-)
      f,( c c c-) 
      f,( c c f,-)
}

\score{
	\notes{ 
%		\shortlong
		\broken
	}
	\paper{ 
	      indent = 0.0\pt
		%for broken!
		linewidth= 30.\mm

	}
}

%% new-chords-done %%
