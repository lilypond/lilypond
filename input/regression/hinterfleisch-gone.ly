\version "1.3.148"
\header {
texidoc="The first and last notes of a measure should be spaced nicely."
}
% jij had zoon mooi tight spacing example,
% kan zo'n harde space ook niet voor en na maatstreep!
\score {
	\notes \relative c'' {
	\time 2/4
	\property Voice.noAutoBeaming=##t
	a4 a8
	a32 a a a
}
%%set bit wide
\paper {linewidth = 100.\mm}
}
