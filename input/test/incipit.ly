%{
 Test of how to make an ``incipit'' to indicate scordatora 
 tuning of a violin part, using the clefStyle property.
 The two first bars of Biber's Rosary sonata III.

   /Mats B
%}

\version "1.3.96";

incipit = \notes\relative c'{
  <b1 fis' b d>
}

emptyincipit = \notes{
 s1
}

violin = \notes\relative c''{
  \specialkey \keysignature f' fis'' g' gis'';
  \time 2/2;
  \clef "treble";

  \key;
  a4. b8 c4 fis |
  gis~ gis8 fis16^\trill ()e b8 c \context Staff<{\voiceOne a d}{\voiceTwo es,4}>|
}

BC  = \notes\relative c{
  \key d \major;
  \time 2/2;
  \clef "bass";

 \key;
  b2. cis4 | 
  d e fis g |
}

\score{
  <
    \context Staff = violin {\notes{
      \property Staff.clefStyle = "transparent" 
      \incipit \bar ".|"; \endincipit
      \violin
    }}
    \context Staff = BC{\notes{
      \property Staff.clefStyle = "transparent" 
      \emptyincipit \bar ".|"; \endincipit
      \BC
    }}
  >
  \paper{
    \translator{\StaffContext
      timeSignatureStyle = "C";
    }
  }
}  

