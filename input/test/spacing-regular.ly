\header
{
 texidoc = "regularSpacingDelta is an experimental feature that
 tries to generate regular spacing for regular notes."
}

\score { \notes \relative c'' {
< \context Staff {

c4 c4 c4 }
\context Staff =SB  { c8 c8   c4 c4 }
>}
\paper{
linewidth = -1.
\translator { \ScoreContext
\consists "Regular_spacing_engraver"

regularSpacingDelta = #(make-moment 1 4 )
}
}
}
