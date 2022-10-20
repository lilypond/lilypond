\version "2.23.81"

\header {
texidoc = "Rests don't change slur direction (default is down).
"
}

\relative c
{
    \clef bass
    \time 2/4
    c16 ( r c) r
    c'( r c) r
    c ( r c,) r
    c ( r c') r
    r ( r r) r r4
}

\relative c
{
    \clef bass
    c8( r c) r
    c'8( r c) r
    c8( r c,) r
    c8( r c') r
    r( r r) r r2
}

\relative c
{
    \clef bass
    c4( r c) r
    c'( r c) r
    c( r c,) r
    c( r c') r
    r( r r) r
}

\relative c
{
    \clef bass
    \time 4/2
    c2( r c) r
    c'( r c) r
    c( r c,) r
    c( r c') r
    r( r r) r
}

