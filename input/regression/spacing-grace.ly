\version "1.7.18"
\header {
  texidoc = "Grace note spacing. Should be tuned? "
}
	
\score {
 \notes \context Voice \relative c'' { \grace {  c16-[ d] } c4 }
  \paper { raggedright = ##t}

}
%% new-chords-done %%
