\version "1.7.18"
\header {texidoc="
covered by slur-attachment-override.  DELETE. "}

fragment = \notes {
  \property Voice.Slur \set #'direction = #1
  \property Voice.Stem \set #'length = #5.5
  g''8(g g4-)
  g4(g8 g-)
  \property Voice.Slur \set #'attachment = #'(stem . stem)
  g8(g g4-)
  g4(g8 g-)
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
