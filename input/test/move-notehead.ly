\version "1.7.18"
% possible rename to scheme- or something like that.  -gp
\header { texidoc = "@cindex Scheme Move Notehead
You can move objects around with scheme.  This example shows how to
move noteheads around. " }

fragment = \notes {
    \outputproperty #(make-type-checker 'note-head-interface)
      #'extra-offset = #'(2 . 3)
    c''2 c
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
