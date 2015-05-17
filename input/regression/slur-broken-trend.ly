
\version "2.19.21"

\header{
texidoc="
Across line breaks, slurs behave nicely.  On the left, they extend to
just after the preferatory matter, and on the right to the end of the
staff.  A slur should follow the same vertical direction it would have
in unbroken state.
"
}

\layout {
    ragged-right = ##t 
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
	   c16 d 
       } }
     >>
    \break 
    c4) f,='2.(\break
    a2.)
    \stemUp
    <d='' d'>8( <c c'> \break
    <bis f' b>) s2..\break
    \stemNeutral

    \new Voice \relative {
	c''8[( c c c] g2 ~ | \break
	g1) | \break
	\slurDown c8[( c] g4 ~ 2 ~ | \break
	g2 ~ 2)
    }
    \relative {
	a'4( c b4 e, ~ | \break
	e1 ~ | 1)
    }
    
}

