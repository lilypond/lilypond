\version "1.7.16"
\header {

texidoc = "setting sparseTies causes only one tie to be
generated per chord pair."

}

	
\score { 
  \context Voice \notes\relative c {
	\property Voice.sparseTies = ##t
	c''  <<c e g>> ~ <<c e g>> 
  }
  \paper {
    raggedright = ##t
  }  
  \midi { }
}
%% new-chords-done %%
