\version "1.7.18"
% TODO: merge with ancient font?
\header {
texidoc="Should use old style."
}

\score {
  \notes { 
    \property Staff.TimeSignature \override #'style = #'neo_mensural
    s1 
  }
}
%% new-chords-done %%
