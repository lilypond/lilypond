
\header {
   tagline="";
}
\version "1.3.5"
\include "paper16.ly";
\score {
  \notes {
    \relative c'' \sequential {
        \clef "violin";
        \time 5/4;
        \key c;
     <c4-\cr( g e>~ <dis ais e> <e, gis b> f )g-\rc
   }
  }
  \paper{
      linewidth = 5.0\cm;
  }
}
