
\header  {
texidoc = "
Texts may be added to the rests by setting @code{text} in
@code{MultiMeasureRestNumber.}. This is done automatically
for the first script specified by @code{R_\markup @{ .. @}}.
"

}

\score { \notes {
 \time 3/4
  R2._\markup { \center << \musicglyph  #"scripts-dfermata" \roman "Ad lib"  >>  }
  R2.^\fermataMarkup
  R2.^"4"
  c2.
}}
