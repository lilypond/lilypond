\version "2.23.5"

\header{
  texidoc = "CSS-style color codes are supported
and must be prefixed with a hash.  In SVG backend,
the given color codes (as hexadecimal strings or
predefined color names) are used directly;
@code{rgb-color} lists are converted to @code{rgb()}
or @code{rgba()} appropriately.

This test's output should be perceivably the same as
@file{input/regression/color.ly}; alpha transparency
is only visible in SVG output.
"
}

\paper { ragged-right = ##t }

darkgreen = "#080"

\relative {
  %% color codes may be passed as a variable.
  \override Accidental.color = \darkgreen
  %% CSS predefined color names may be used. Names are
  %% case-insensitive
  \override Beam.color = "aqUA"
  %% rgb-color lists are supported, and may
  %% include an alpha channel.
  \override NoteHead.color = #(rgb-color 0.5 0.5 0 1)
  c'4
  %% Color codes are case-insensitive.
  \override NoteHead.color = "#ffDab9"
  f
  %% Above and below should be equivalent.
  \override NoteHead.color = "peachPUFF"
  f
  %% CSS three-chars shorthand is supported.
  \override NoteHead.color = #"#808"
  g
  %% Extraneous chars are ignored.
  \override NoteHead.color = #"#000088FFEDCBA"
  b
  %% ly:stencil-in-color works:
  \override NoteHead.stencil =
  #(lambda (grob)
     (ly:stencil-in-color
            (ly:note-head::print grob)
            "#00ff00"))
  %% stencil-with-color works:
  \override Stem.stencil =
  #(lambda (grob)
     (let ((st (ly:stem::print grob)))
       (if (ly:stencil? st)
           (stencil-with-color st "#00F")
           empty-stencil)))
  \override Flag.color = "#F0F"
  e8 es d dis e4
}

\layout {
  \context {
    \Staff
    %% Alpha transparency is enabled in SVG output.
    \override Clef.color = "#0008"
  }
}
