\version "1.7.16"


fragment = \notes {
  a''^"3 $\\times$ \\`a deux"
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
