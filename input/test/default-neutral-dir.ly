\version "1.7.18"
% delete this; it's covered by beam-neutral-direction.ly

% old texidoc:
%The direction of a perfectly centred beams can be
%controlled through @code{Voice.Beam}'s grob-property
%directly@code{neutral-direction}
\header { texidoc = "DELETE ME.
"}

\paper { raggedright = ##t}
\score {\notes \relative c {
   b''8-[ b]
  \property Voice.Beam \set #'neutral-direction = #-1
   b-[ b]
}}
%% new-chords-done %%
