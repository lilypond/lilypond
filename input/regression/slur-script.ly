
\header {

    texidoc = "A slur avoids collisions with scripts.  Only articulations
    go inside the slur, dynamic markings, fingerings texts etc. go outside
    of slur.

    For different configurations, the scripts can be moved manually."
    }

\version "2.3.16"
\paper {
    raggedright = ##t
}
\relative
{
    b4-.( b-. b-.)
    b(-.  d-.)

    b_1( b b_1_2_3)
    c-1_\markup { \italic {"cresc."}}( f c)
    c-1( c\f c)
    \once \override Script #'padding = #1.2
    b-.( b-.
    \once \override Script #'padding = #1.2
    b-.)
    e=''16[-.( f-.)]
}
