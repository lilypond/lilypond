\header{
% what's the problem?
% well, that's elementary my dear watson
texidoc="The decrescendo disappears when the part combiner decides that
the a2 and c2 should be a chord in one voice.  Using the commented
version, with a c2 instead of a2, the decrescendo reappears."
}
\version "1.3.148"

\score{
	\context Staff <
		\context Voice=one \skip 1
		\context Voice=two \skip 1
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				c2 \clef bass c2
				c2 c2
			}
			\context Thread=two \notes\relative c'' {
				b2 \< a4 () \! a
				%c2 \> a4 () \! a
				a2 \> a4 () \! a
			}
	>
	\paper{
		linewidth=140.\mm
	}
}


