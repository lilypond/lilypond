\version "1.5.68"

\header{
texidoc = "Hairy case for beam, chord, and automatic knees."

%texidoc = "Beam thinks that first two notes should be stem down.  Can
%be fixed by uncommenting \stemUp"
}

\score{
  \notes\relative c'{
    %\stemUp
    \clef alto
    \time 3/4
    r8 <d ( bes > ) bes' d <e-> g, c, c,> r |
  }
}
