\version "1.7.18"
% regression or delete.  -gp
\header{ texidoc = "" }

\score { 
  \context Voice \notes\relative c {
    \context Voice=VoiceOne
	\grace c8 d4 d d d d
	\grace { e16 f e f } d4 d d d d 
	
  }
  \paper { }  
  \midi { }
}
%% new-chords-done %%
