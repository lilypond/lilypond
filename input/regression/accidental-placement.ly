\header {
    texidoc ="Accidentals are placed as closely as possible.
Accidentals in corresponding octaves are aligned."
}


\score { \notes \context Voice \relative c' {
    cis4
    <cis4 d es fis gis ases bes ces d dis >
    <bes'! fis!>     
    <bes! cis!>
    <c! es ges beses>
    <bes! c d f >    
}
\paper { linewidth = -1. }
     }
