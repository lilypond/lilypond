\version "1.7.18"
\header {
texidoc = "@cindex Boxed Molecule
You can override the molecule callback to draw a box around
arbitrary grobs.

TODO:  circled molecules."
% TODO: check that this one actually works.
}


 \score { \notes \relative c''  {

 \property Voice.TextScript \override #'molecule-callback =
   #(make-molecule-boxer 0.1 0.3 0.2 Text_item::brew_molecule)

   c'4^"foo"

\property Voice.Stem \override #'molecule-callback =
   #(make-molecule-boxer 0.05 0.25 0.25 Stem::brew_molecule)

\property Score.RehearsalMark \override #'molecule-callback =
   #(make-molecule-boxer 0.15 0.3 0.3 Text_item::brew_molecule)

      c8
\property Voice.Stem \revert #'molecule-callback

      c4. c4 \mark "F" c1 
   }}
%% new-chords-done %%
