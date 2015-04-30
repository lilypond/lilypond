\version "2.19.8"

\header {
  texidoc = "Controlling the appearance of tremolo slashes.  Property
  @code{slope} is self-explanatory.  Property @code{shape} determines
  whether slashes look like rectangles or like very small beams.  Setting
  these properties directly cause all slashes behave in the specified way.
  However, one usually wants the slashes to behave differently depending on
  whether the note has flags, beams or only a plain stem.  That's what the
  @code{style} property is used for: it sets shape and slope depending on
  the situation.  There are two styles defined: @code{default} and
  @code{constant}."
}

music = {
  a''4:32 a':
  e''8: \noBeam e':
  a'': [ a': ]
  f': [ g':]
  d': [ d': ]
}

\markup \wordwrap { default.  First three notes should have beam-like slashes.
Slash of the third note should be more sloped than first two notes.
Slashes on beamed notes should be rectangular and parallel to the beams. }
\new Staff {
  \music
}

\markup \wordwrap { style=constant.  All slashes should be rectangular.
All slashes should have the same slope except for downstem flagged notes. }
\new Staff {
  \override StemTremolo.style = #'constant
  \music
}

\markup \wordwrap { shape=rectangle.  All slashes should be rectangular.
Slope like in default. }
\new Staff {
  \override StemTremolo.shape = #'rectangle
  \music
}

\markup \wordwrap { shape=beam-like.  All slashes should be beam-like.
Slope like in default. }
\new Staff {
  \override StemTremolo.shape = #'beam-like
  \music
}

\markup \wordwrap { slope=-0.2  All slashes should have the same downward slope.
  Shape like in default. }
\new Staff {
  \override StemTremolo.slope = #-0.2
  \music
}
