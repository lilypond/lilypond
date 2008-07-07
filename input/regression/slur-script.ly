
\header {

  texidoc = "A slur avoids collisions with scripts.  Articulations
    go inside the slur, dynamic markings go outside the slur.
    Fingerings and texts are placed either inside or outside.

    For different configurations, the defaults can be changed, and
    scripts can be moved manually."
}

\version "2.11.51"
\layout {
  indent = 0\mm
  ragged-right = ##t
}
\relative
{
  b4-.( b-. b-.)
  b(-.  d-.)

  b_1( f'_1_2_3 c_3_4_5 a)

  \override DynamicLineSpanner #'avoid-slur = #'outside
  b(^"dyn outside" b f'\p b,)
  
  %% Do not force dynamics outside slur, only avoid collisions.
  \override DynamicLineSpanner #'avoid-slur = #'around
  b(^"dyn around" e\p e\f b)
  
  %% Do not avoid collisions.
  \override DynamicLineSpanner #'avoid-slur = #'()
  b(^"no avoid" d\p d\f b)
  
  c_\markup { \italic {"cresc."}}( c c)
  \once \override Script #'padding = #1.2
  b-.( b-.
  \once \override Script #'padding = #1.2
  b-.)
  e='16[-.( f-.)]
}
