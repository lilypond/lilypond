
\version "2.3.12"

\header {

    texidoc="
Extreme slurs are scaled to fit the pattern, but only  symmetrically.
Asymmetric slurs are created by setting @code{excentricity}."

}
\paper {
    raggedright = ##t
}

baseWalk =  \relative c {
    \time 6/4
    \stemDown
    \slurUp
    d,8[( a' d f] a[ d f d] a[ f d  a)]
    d,8[( a' d f] a[ a a d] f[ d d,  a)]
    \once\override Slur #'excentricity = #6.0
    d,8[( a' d f] a[ a a d] f[ d d,  a)]
}

\context PianoStaff 
    \autochange \baseWalk


