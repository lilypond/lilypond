
\version "2.3.4"

\header{
texidoc="
Across line breaks, slurs behave nicely.  On the left, they extend to
just after the preferatory matter, and on the right to the end of the
staff.  A slur should follow the same vertical direction it would have
in unbroken state.
"
}
\paper {
    linewidth=40.\mm
    indent=0.
}

\relative c''{
    e1( \break a,)
    \time 2/4
    e'2( \break a,)(\break
    a2\break
    e'2)
    \time 4/4
    << d1_(\trill
       { s2 \grace {
	   c16[ d] 
       } }
     >>
    \break 
    c4)
}

