\version "1.7.6"
\header {
  texidoc = "Grace note spacing. Should be tuned? "
}
	
\score {
 \notes \context Voice \relative c'' { \grace { [c16 d] } c4 }
  \paper { linewidth =-1. }

}
%% new-chords-done %%
