\header { texidoc = "
The direction of a perfectly centred beams can be
controlled through @code{Voice.Beam}'s grob-property
@code{default-neutral-direction}
";}

\paper { linewidth = -1.;}
\score {\notes \relative c {
  [b''8 b]
  \property Voice.Beam \set #'default-neutral-direction = #-1
  [b b]
}}
