
\score {\notes \context Voice = VA \relative c'' {
	\grace b8 c4-\fermata
	\grace { [c32 cis32] } gis4
	\grace { [cis32 dis32] } e4
	\grace { [c32 d] }\times 2/3 { [c8 c c] }
	 \grace { [b32 ( c32] } ) c4
	\grace  <c16 d16> [c8 c8]
%	\grace  c16 [c8 c8]
	\grace  { \property Grace.graceAlignPosition = \right c16} c4
}
\paper {linewidth = -1.;}
%%\midi{ }
}
