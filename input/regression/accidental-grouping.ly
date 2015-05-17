\version "2.19.21"

\header {
  texidoc = "By setting @code{accidentalGrouping} to @code{'voice},
LilyPond will horizontally stagger the accidentals of octaves
in different voices as seen in this test's E-sharp.
"
}

 \relative {
   << { <gis'' cis eis gis>1 } \\
      { eis,              } >>
  \set Staff . accidentalGrouping = #'voice
   << { <gis' cis eis gis>1 } \\
      { eis,              } >>
}
