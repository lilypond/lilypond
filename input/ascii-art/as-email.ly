% ASCII Art output
%
% Process as:
%
%     lilypond -fas foo.ly
%     as2text foo.as

\include "paper-as5.ly"

\score {
  \notes\relative c'' {
    \time 3/8;
    a8 a a 
    \time 5/8;
    a a a a a
    %a16 a  a a a a a a a a 
  }
  \paper {
    linewidth = -1.;
    \translator {
      \StaffContext
      StaffSymbol \override #'molecule-callback = #(lambda (x) '())
    }
  }
}

