\version "1.7.16"
\header { texidoc = "
The direction of a perfectly centred beams can be
controlled through @code{Voice.Beam}'s grob-property
directly@code{neutral-direction}
"}

\paper { raggedright = ##t}
\score {\notes \relative c {
  [b''8 b]
  \property Voice.Beam \set #'neutral-direction = #-1
  [b b]
}}
%% new-chords-done %%
