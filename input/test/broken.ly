\version "1.7.18"

\header{ texidoc = "Broken parts of slurs should show the trend of the
encompassed notes."
}
shortlong =  \notes{
	c4(c-)( c c  |
	c c c c |
	c c c c |
	\break
	c c c c-) |
}

broken =  \notes\transpose c c'{
      c c c c(
      c c-) c c(
      a'-) a' a' a'()
      a' a' a' a'(
}

\score{
	\notes{ 
		\broken
	}
	\paper{ 
	      raggedright = ##t
	}
}

%% new-chords-done %%
