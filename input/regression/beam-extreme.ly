\version "1.3.146"
\header{
texidoc="
Beams should behave reasonably well, even under extreme circumstances.
Stems may be short, but noteheads should never touch the beam.  Note that
under normal circumstances, these beams would get knees here
Beam.auto-knee-gap was set to false.
"
}

extreme = \notes\relative c'' {
  \stemBoth
  [g8 c c,]
  [c16 c'' a f]
  \stemUp 
  [c,,32 c'' a f]
  r4
}
\score {
  {
    % If we want to test extreme beams,
    % we should not have them auto-kneed
    \property Voice.Beam \override #'auto-knee-gap = ##f
    \extreme
    \property Voice.Beam \override #'slope-limit = #1000
    \extreme
  }
  \paper{
    linewidth=-1.
  }
}
