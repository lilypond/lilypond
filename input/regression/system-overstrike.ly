\version "2.3.16"
\header { texidoc = "By setting betweensystempadding to a negative
value, it is possible to eliminate the anti-collision constraints.
Then  setting @code{betweensystemspace} to a low (nonzero) value,
print  systems in overstrike. "
}

\bookpaper {
    raggedbottom =##t
    betweensystempadding = - 4\cm
    betweensystemspace = 3\mm
}

{
    c1 \break
    c'''1
}

