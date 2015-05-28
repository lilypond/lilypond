
\header {
  texidoc = "The @code{toward-stem-shift} property controls the precise
horizontal location of scripts that are placed above an upstem or below
a downstem note (@code{0.0} means centered on the note head, @code{1.0}
means centered on the stem).
"
}

\version "2.19.21"
\relative
{
  \override Script.toward-stem-shift = #1.0
  a'4^> c_>

  \override Script.toward-stem-shift = #0.0
  a4^> c_>
}
