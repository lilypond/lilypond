

%{

This is taken from [Gourlay]'s paper on breaking lines

%}

\score{
       \notes \context Staff  \relative c'' <
               \context Voice = VA { \stemUp d2 d     | d d | d4 d2. | \break  c1 }
	       \context Voice = VB { \stemDown g4 g g g | \times 2/3 { g2 g2 g2 } | g4. g8 g2 | c1 }
               >
       \paper{
           linewidth = 9.\cm;
       }
}

