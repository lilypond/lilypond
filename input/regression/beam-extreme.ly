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

  %%%%%%%
  \stemBoth
  \times 2/3{[d16 fis' d,]} \times 2/3{[cis g'' cis,,]}
  a'16 cis a, g''' % Used to give a nice beam directed upwards.
  \stemBoth

  \transpose c' {
	  \stemDown [e'8 e e']
      }
}

\score {
  {
    % If we want to test extreme beams,
    % we should not have them auto-kneed
    \property Voice.Beam \override #'auto-knee-gap = ##f
    \extreme


    %% what does this test? --hwn
%{
    \property Voice.Beam \override #'slope-limit = #1000
    \extreme
    %}
  }
  \paper{
%    raggedright = ##t
    linewidth = -1
  }
}
