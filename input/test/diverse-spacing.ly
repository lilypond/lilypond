\version "1.7.18"
% I think this one could be removed or regression'd.
\header{texidoc="REGRESSION or DELETE " }
% a few 32nds shouldn't stretch spacing enormously.
\score {
\notes { \time 3/4
\relative c'{ 
  g'8. c16 es8. d16 c8. bes32 as g8. c,16
  es4 r8 es |
   d-[ es f g as c ]
  b4 g r
}

}
\paper  { raggedright = ##t}
}
%% new-chords-done %%
