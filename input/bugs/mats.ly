
corI=\notes\relative c'' {
\key c;

[g8. \f ( a16 ] ) g2 |
[g8.  ( a16 ] ) g2_"dim." |
c2. \p ( |
) g2 g4 ( |
) c2. ( |
) g4 [g8. ( a16 ] ) g4 |
c2. \p ( |
) g2 g4 |
c2 \< ( ~ [ \! c8 \> \! ) g ] |
g2 \p r4 |
g \mf ( d' c~ |
c ) b r |
R2. |
}

corII=\notes\relative c'' {
\key c;
\time 3/4;

R2.*19 |
r4 r [g8^"solo" \p (\< \! ) e'] |
g2 \> ( [e8. ) \! c16 ] | % grace note e8 () g2
g2 r 4 |
r r \times 2/3 {[e'8 \p ( g, ) g' ]} |
g4 \> () \! e r |
r r \times 2/3 {[g8 \p ( e ) g ]} |
d4 \> () \! g r |
r r [g,16 ( \p d' e d ] |
) g,4 r r |
R2. |
r4 r c \p ( |
) g r r |
g [g8. ( a16 ] ) g4 |
R2. |
r4 r [g8^""^"solo" \mf \< () \! e' ] |
g2 \> [ \! e8. ( c16 ] |
) g2 r4 |
R2. |
r4 r \times 2/3 {[e'8 \f ( g, ) g'] } |
g4 () e r |
r r \times 2/3 {[g8 \f ( e ) g] } 
d4 () g r |
r r [d16 \f ( g, e' d ] |
[g, d' e ) d ] g,4 r |
r d'2 \f \> ( | % grace note g8 d2
[g,8. a16 g8. a16 \! g8. ) a16 ] |
g4 r r |
}

trpI=\notes\relative c'' {
\key c;
\time 3/4;

R2.*30 |
}

\score{ <
  \type StaffGroup = brass <
    \type Staff = cor <
      \type Voice = corI { \stemup \corI }
      \type Voice = corII { \stemdown \corII }
    >
    \type Staff = trp <
      \type Voice = trpI { \stemup \trpI }
    >
  >
>
 \paper {
    \translator {\OrchestralScoreContext
	% The following line causes a SIGSEGV
	\consists "Multi_measure_rest_engraver"; 
    }
  }
}
