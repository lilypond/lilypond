\version "2.4.0"
\header { texidoc = "By setting betweensystempadding to a negative
value, it is possible to eliminate the anti-collision constraints.
Then  setting @code{betweensystemspace} to a low (nonzero) value,
print  systems in overstrike.

Unfortunately, this does not show in the colllated texinfo document. Run this example stand-alone to see the effect.
"
	  
}

\paper {
    raggedbottom =##t
    betweensystempadding = - 4\cm
    betweensystemspace = 3\mm
}

{
    c1 \break
    c'''1
}

