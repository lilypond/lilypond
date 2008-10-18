\version "2.11.61"
\header {
  lsrtags = "rhythms,tweaks-and-overrides"
  texidoc = "
The text used for crescendos and decrescendos can be changed by
modifying the context properties @code{crescendoText} and
@code{decrescendoText}.  The style of the spanner line can be
changed by modifying the @code{'style} property of
@code{DynamicTextSpanner}.  The default value is @code{'hairpin},
and other possible values include @code{'line}, @code{'dashed-line}
and @code{'dotted-line}:
"
  doctitle = "Changing text and spanner styles for text dynamics"
}

\relative c'' {
  \set crescendoText = \markup { \italic { cresc. poco } }
  \set crescendoSpanner = #'text
  \override DynamicTextSpanner #'style = #'dotted-line
  a2\< a
  a2 a
  a2 a
  a2 a\mf
}
