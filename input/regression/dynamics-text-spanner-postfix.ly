\version "2.13.4"

\header {
texidoc = "The \cresc, \dim and \decresc spanners can now be redefined as
postfix operators and produce one text spanner.  Defining custom spanners is
also easy.  Hairpin and text crescendi can be easily mixed. \< and \> produce
hairpins by default, \cresc etc. produce text spanners by default."
}

% Some sample text dynamic spanners, to be used as postfix operators
crpoco = #(make-music 'CrescendoEvent 'span-direction START
                      'span-type 'text 'span-text "cresc. poco a poco")
% Redefine the existing \cresc, \dim and \decresc commands to use postfix syntax
cresc = #(make-music 'CrescendoEvent 'span-direction START
                     'span-type 'text 'span-text "cresc.")
dim = #(make-music 'DecrescendoEvent 'span-direction START
                   'span-type 'text 'span-text "dim.")
decresc = #(make-music 'DecrescendoEvent 'span-direction START
                       'span-type 'text 'span-text "decresc.")

\relative c' {
  c4\cresc d4 e4 f4 |
  g4 a4\! b4\crpoco c4 |
  c4 d4 e4 f4 |
  g4 a4\! b4\< c4 |
  g4\dim a4 b4\decresc c4\!
}
