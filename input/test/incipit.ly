%{
 Test of how to make an ``incipit'' to indicate scordatora 
 tuning of a violin part, using the clefStyle property.
 The two first bars of Biber's Rosary sonata III.

   /Mats B
%}

\version "1.3.42";

incipit = \notes\relative c'{
  <b1 fis' b d>
}

violin = \notes\relative c''{
  \specialkey \keysignature f' fis'' g' gis'';
  \time 2/2;

  \key;
  a4. b8 c4 fis |
  gis~ gis8 fis16^\trill ()e b8 c \context Staff<{\voiceone a d}{\voicetwo es,4}>|
}

BC  = \notes\relative c{
  \key d;
  \time 2/2;
  \clef "bass";

 \key;
  b2. cis4 | 
  d e fis g |
}

\score{
  \notes{
    \context Staff=violin
    \property Staff.clefStyle = "transparent" 
      \incipit 
    < \context Staff=violin { 
      \bar ".|"; \endincipit
      \violin}
      \context Staff=cb { \property Staff.clefStyle = "transparent" 
      \bar ".|";  \endincipit 
      \BC}>
  }
  \paper{
    \translator{\StaffContext
      timeSignatureStyle = "C";
    }
  }
}  

