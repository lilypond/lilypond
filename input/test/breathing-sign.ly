\version "1.3.42";

\score {
  \notes \relative c' {
    \key es; \time 3/4;
    < \context Voice = two { \stemdown es4 bes es }
      \context Voice = one { \stemup g4 as g }
    > |
    < \context Voice = two { \stemdown es4 \breathe bes es }
      \context Voice = one { \stemup g4 as g }
    > |
    es8 d es f g4 \breathe |
    es8 d \breathe es f g f |
    es2 r4 \bar "||";
  }
}
