%{
 Test of how to make an ``incipit'' to indicate scordatora 
 tuning of a violin part, using the clefStyle property.
 The two first bars of Biber's Rosary sonata III.

   /Mats B
%}

\version "1.0.14";

incipit = \notes\relative c'{
  <b1 fis' b d>
}

violin = \notes\relative c''{
  \specialkey \keysignature f' fis'' g' gis'';
  \time 2/2;

  a4. b8 c4 fis |
  gis~ gis8 fis16^\trill ()e b8 c \type Staff<{\voiceone a d}{\voicetwo es,4}>|
}

\score{
  \notes{
    \property Staff.clefStyle = "transparent" 
    \incipit 
    \property Staff.clefStyle = "fullSizeChanges" \clef "treble"; \bar "";
    \violin
  }
  \paper{
    \translator{\StaffContext
      timeSignatureStyle = "C";
    }
  }
}  

