
\header{
texidoc="Two automatic knees";
}

\score {
  \notes \context PianoStaff <
    \context Staff = "up" \notes\relative c''{
      [ b8 \translator Staff="down" d,, ]
      [ c \translator Staff="up" c'' ]
      [ b, \translator Staff="down" d ]
    }
    \context Staff = "down" {
      \clef bass; 
      s2.
    }
  >
  \paper{
    linewidth = 40*\staffspace;
% Now by default
%    \translator{
%      \VoiceContext
%      Beam \override #'auto-knee-gap = #7
%    }
  }
}
