\version "1.7.16"

\header{
texidoc="
Slurs can be forced to always attach to note heads.
"
}

fragment = \notes {
  \property Voice.Slur \set #'direction = #1
  \property Voice.Slur \set #'attachment = #'(head . head)
  g''16()g()g()g()d'()d()d()d
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
