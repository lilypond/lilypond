\header {
texidoc="The two sharps overstrike, the flat and sharp overstrike"
}
\version "1.3.148"

\include "paper16.ly"
\score {
  \notes {
    \relative c'' \context Voice \sequential {
     <dis1 ais e> <bes dis>
   }
  }
  \paper{
      linewidth = 5.0\cm
  }
}
