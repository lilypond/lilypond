\header{
enteredby =  "Donat Wullschleger";
}

\version "1.3.93";

\include "paper16.ly";
\include "deutsch.ly";

righthandfirstvoice =  \notes \relative c''
 \context Voice = upper {
  \key b \major;
  \time 6/8;

  r8. r8. r8. r8. |
  r4. r4. |
  r2. |

  \bar "|.";
}

lefthandfirstvoice =  \notes \relative c
   \context Voice = upper {
  \voiceOne
  \key b \major;
  \clef bass;

  as4. [g8 a16 h c8] |
  as4. [g8 a16 h c8] |
  as4. [g8 a16 h c8] |
}

lefthandsecondvoice =  \notes \relative c
   \context Voice = lower {
  \voiceTwo
  r2. |
  r4. r4. |
  r8. r8. r8. r8. |
}

\score{ 
 \context PianoStaff \notes <
  \context Staff = top <
  \righthandfirstvoice
  >
  \context Staff = bottom <
  \lefthandfirstvoice
  \lefthandsecondvoice
  >
  > 

 \paper{
  \translator{
          \OrchestralScoreContext
   }

 }
}
