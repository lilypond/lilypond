
\include "paper16.ly"
\include "deutsch.ly"

melodie = \notes \relative c' {
  d2 d4 e f2 f g4 f e d e2 e \breathe |
  f d4 e f g a2 b g4 g f2 r2 |
  c'2 c4 c a2 d c4 b a g f2 g \breathe |
  a d,4 d g2 g f4 d e2 e d2-\fermata \bar "|.";
}

righthand = \notes \relative c' {
  a2 a4 cis4 | d2 d2 | d4 c2 a4 | d4 h cis2 \breathe |
  d2 a4 c4 | c4 e f2 | f2 f4 e | c2 r2 |
  e4 f g2 | f2 f2 | e4 d f e | d2. e4 \breathe |
  f4 d b4 b | d2 d | d4 c d h | cis2 a2 |
}

lefthand = \notes \relative c {
  f2 f4 a | a2 a2 | b4 a g f | a2 a \breathe |
  a2 f4 g | a c c2 | d2 c4 c | a2 r2 |
  g2 c4 c | c2 b2 | g4 f c' c | a4 b2 cis4 \breathe |
  d4 a f f | b2 b | a4 a a2 | a2 fis2 |
}

pedal = \notes \relative c {
  d2 d4 a d2 d g,4 a c d a2 a \breathe |
  d2 d4 c f c f2 b,4 g c c f2 r2 |
  c4 d e c f2 b, c4 d f c d b a2 \breathe |
  d4 c b2 g4 a b2 d2 a a d_\fermata \bar "|.";
}

\score {
  <
    \context PianoStaff <
      \context Staff = treble <
	\notes { \key f; \time 2/2; }
	\context Voice = melody {
	  \stemup \melodie
	}
	\context Voice = right {
	  \stemdown \righthand
	}
      >
      \context Staff = bass {
	\notes { \key f; \clef "bass"; }
	\lefthand
      }
    > 
    \context Staff = pedal {
      \notes { \key f; \clef "bass"; }
      \pedal
    }
  >

  \header {
    title = "Von guten Mächten treu und still umgeben";
    composer = "Satz: Michael Krause 1999 (*1977)";
    enteredby = "Michael Krause";
    copyright = "dunno";
    poet = "Dietrich Bonhoeffer 1944";
  }

  \paper{
%    \paper_sixteen
%    linewidth = 18.0 \cm;
%    textheight = 28.0 \cm;
    indent = 0.0 \mm;
    \translator { \OrchestralScoreContext }
  }
}

\score {
  \context StaffGroup <
    \context Staff = treble <
      \notes { \key f; \time 2/2; }
      \context Voice = sop {
	\stemup \melodie
      }
      \context Voice = alt {
	\stemdown \righthand
      }
    >
    \context Staff = bass <
      \notes { \key f; \clef "bass"; }
      \context Voice = ten {
	\stemup \lefthand
      }
      \context Voice = bas {
	\stemdown \pedal
      }
    >
  > 

  \header {
    title = "Von guten Mächten treu und still umgeben";
    composer = "Satz: Michael Krause 1999 (*1977)";
    enteredby = "Michael Krause";
    copyright = "dunno";
    poet = "Dietrich Bonhoeffer 1944";
  }

  \paper{
%    \paper_sixteen
%    linewidth = 18.0 \cm;
%    textheight = 28.0 \cm;
    indent = 0.0 \mm;
  }
}

