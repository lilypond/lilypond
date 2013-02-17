\version "2.17.10"

\header {
  texidoc = "By setting @code{accidentalGrouping} to @code{'voice},
LilyPond will horizontally stagger the accidentals of octaves
in different voices as seen in this test's E-sharp.
"
}

 \relative c''' {
   << { <gis cis eis gis>1 } \\
      { eis,              } >>
  \set Staff . accidentalGrouping = #'voice
   << { <gis' cis eis gis>1 } \\
      { eis,              } >>
}
