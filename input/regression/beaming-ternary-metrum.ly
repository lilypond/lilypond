\version "1.7.16"
\header {
texidoc = "automatic beaming also works in ternary time sigs."
}

\score {
   \notes \context Staff {
\time 6/8
 c8.-[ c16 c16 c16] 
}
\paper { raggedright = ##t}
}
%% new-chords-done %%
