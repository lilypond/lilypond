\version "1.5.68"

\header{
texidoc="
Across line breaks, slurs behave nicely.  On the left, they extend to
just after the preferatory matter, and on the right to the end of the
staff.  A slur should follow the same vertical direction it would have
in unbroken state.
"
}
\score{
	\notes \relative c''{
		e1( \break) a,
		\time 2/4
		e'2( \break) a,(\break
		a2\break
		)e'2
	}
	\paper {
		linewidth=40.\mm
		indent=0.
	}
}
