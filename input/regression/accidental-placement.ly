#(ly:set-option 'old-relative)
\version "1.9.1"

\header {

    texidoc ="Accidentals are placed as closely as possible.
Accidentals in corresponding octaves are aligned.
The top accidental should be nearest to the chord. The
flats in a sixth should be staggered.  "
    
}


\score { \notes \context Voice \relative c' {
    cis4
    c4
 \transpose c c' {    
  <<bis es gis>>4
  <<es! as!>>
  <<gis! cis!>>
  <<g! des'>>
  <<ges! es'!>>
}    
    <<cis d es fis gis ases bes ces d dis >>4
    <<bes'! fis!>>     
    <<bes! cis!>>
    <<c! es ges beses>>
    <<bes! c d f >>    
    <<bes,! c d f >>
}
\paper { raggedright = ##t}
     }


