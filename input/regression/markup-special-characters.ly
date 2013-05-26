\version "2.17.19"
\header {
  texidoc = "
  A list of special character ASCII aliases can be easily included.
  This works for markups and lyrics.
"
}

\paper {
  #(include-special-characters)
  indent = 0
}

\markup \column {
  \bold "Markup example:"
  "Input:"
  \override #'(replacement-alist . ()) \typewriter \justify {
    &numero;2 &ndash; &OE;dipe&hellip;
  }
  "Output:"
  \italic \justify {
    &numero;2 &ndash; &OE;dipe&hellip;
  }
  \combine \null \vspace #0.5
  \bold "Lyric example:"
}
\new Lyrics \lyricmode {
  Ce&s;16 -- &s;ez In -- fi -- dè -- les, un c&oe;ur in -- no -- cent
  ne craint rien&nnbsp;;
}
