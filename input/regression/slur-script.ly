
\header {

    texidoc = "A slur avoids collisions with scripts.  Articulations
    go inside the slur, dynamic markings go outside the slur.
    Fingerings and texts are placed either inside or outside.

    For different configurations, the defaults can be changed, and
    scripts can be moved manually."
  }

\version "2.6.0"
\layout {
    raggedright = ##t
}
\relative
{
    b4-.( b-. b-.)
    b(-.  d-.)

    b_1( f'_1_2_3 c_3_4_5 a)
    b( f'\p b,)
    
    %% Do not force dynamics outside slur, only avoid collisions.
    \override DynamicLineSpanner #'avoid-slur = #'()
    \override DynamicLineSpanner #'padding = #0.3
    b( e\p e\f b)
    
    c_\markup { \italic {"cresc."}}( c c)
    \once \override Script #'padding = #1.2
    b-.( b-.
    \once \override Script #'padding = #1.2
    b-.)
    e='16[-.( f-.)]
}
