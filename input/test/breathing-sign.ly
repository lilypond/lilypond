\version "1.3.110";

\score {
  \notes \relative c' {
    \key es \major; \time 3/4;
    < \context Voice = two { \stemDown es4 bes es }
      \context Voice = one { \stemUp g4 as g }
    > |
    < \context Voice = two { \stemDown es4 \breathe bes es }
      \context Voice = one { \stemUp g4 as g }
    > |
    es8 d es f g4 \breathe |
    es8 d \breathe es f g f |
    es2 r4 \bar "||";
  }
}
