
%
% huh? wat gaat er nu fout in os-score.ly, hier issie okede?
%
global = {
  \time 2/4;
  \skip 2*4; \bar "|.";
}
Key = \notes \key as \major;

\score {
    <
    \global
    \context StaffGroup = timpani <
      \context Staff = timpani <
    \Key
    \notes\relative c {
    \times 2/3 { f4 f f }
    \times 4/5 { as8 as as as as }
    R1
  }
  > >
  >
  \paper {
    linewidth = -1.;
    \translator {
      \HaraKiriStaffContext
    }
  }
}
