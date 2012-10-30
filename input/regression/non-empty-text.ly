\version "2.17.6"

\header{
  texidoc = "By default, text is set with empty horizontal dimensions.
The property @code{extra-spacing-width} in @code{TextScript} is used
to control the horizontal size of text."
}

\layout {
  line-width = 3.0\cm
}

\relative c''{
  %% \textLengthOff
  %% short for \override TextScript.extra-spacing-width = #'(+inf.0 . -inf.0)
  %%           \override TextScript.extra-spacing-height = #'(0 . 0)
  c2_"very wide and long text" c | \break
  %% short for \override TextScript.extra-spacing-width = #'(0 . 0)
  %%           \override TextScript.extra-spacing-height = #'(-inf.0 . +inf.0)
  \textLengthOn
  c2_"very wide and long text" c
}
