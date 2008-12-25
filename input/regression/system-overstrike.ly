\version "2.12.0"
\header { texidoc = "By setting between-system-padding to a negative
value, it is possible to eliminate the anti-collision constraints.
Then  setting @code{between-system-space} to a low (nonzero) value,
print  systems in overstrike.

Unfortunately, this does not show in the colllated texinfo document. Run this example stand-alone to see the effect.
"
	  
}

\paper {
    ragged-bottom =##t
    between-system-padding = - 4\cm
    between-system-space = 3\mm
}

{
    c1 \break
    c'''1
}

