%
% mmrests of second voice should not disappear
%
% the problem is more complex: the mmrest-engraver lives at staff level,
% but it seems that we need one per voice.
%
\score {
  \notes <
    \context Staff = Viole <
	\context Voice=one \partcombine Voice
		\context Thread=one \relative c''{ R1 d4 d d d }
		\context Thread=two { R1*2 }
    >
  >
  \paper {
% {
    \translator {
      \StaffContext
      \remove Multi_measure_rest_engraver;
      \remove Bar_engraver;
    }
    \translator {
      \VoiceContext
      \consists Multi_measure_rest_engraver;
      \consists Bar_engraver;
    }
% }
  }
}