
\version "2.3.22"

\header{
texidoc = "Hairy case for beam, chord, and automatic knees."

%texidoc = "Beam thinks that first two notes should be stem down.  Can
%be fixed by uncommenting \stemUp"
}

\layout { raggedright= ##t }


\score{
  \relative c'{
    %\stemUp
    \clef alto
    \time 3/4
    r8 <d  bes >( bes') d <e g, c, c,>-> r |
  }
}
