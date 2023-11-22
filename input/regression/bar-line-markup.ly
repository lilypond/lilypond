\version "2.25.11"

\header {
  texidoc = "The markup command @code{\\bar-line} scales nicely with different
@code{\\fontsize}.  It is customizable by overriding @code{height},
@code{dot-count}, @code{dash-count}, @code{kern} or @code{thick-thickness}.
The brace bar line sometimes emits a warning, if none exactly fitting can be
found (this warning is not silenced here)."
}

\markup {
  \override #'(word-space . 2)
  \column {
    \line {
      "default (fontsize 0):" \bar-line "]{.|:;!S!;:|.}["
    }
    \line {
      \hspace #4
      "fontsize: -7" \fontsize #-7 \bar-line "]{.|:;!S!;:|.}["
      "fontsize: 7" \fontsize #7 \bar-line "]{.|:;!S!;:|.}["
    }
    \draw-hline
    %\vspace #1
    \line {
      "default (height 4):" \bar-line "]{.|:;!S!;:|.}["
    }
    \line {
      \hspace #4
      "height: 2" \override #'(height . 2) \bar-line "]{.|:;!S!;:|.}["
      "height: 6" \override #'(height . 6) \bar-line "]{.|:;!S!;:|.}["
    }
    %\vspace #1
    \draw-hline
    \line {
      "default (dot/dash-count 4/5):" \bar-line ";|!"
    }
    \vspace #1
    \line {
      \hspace #4
      "dot-count (3/5):"
      \override #'(dot-count . 3) \bar-line ";|!"
      \override #'(dot-count . 5) \bar-line ";|!"
      "dash-count (3/7):"
      \override #'(dash-count . 3) \bar-line ";|!"
      \override #'(dash-count . 7) \bar-line ";|!"
    }
    %\vspace #1
    \draw-hline
    \line {
      "default (kern 3.0/hair-thickness 1.9/thick-thickness 6.0):"
      \bar-line "|."
    }
    \vspace #1
    \line {
      \hspace #4
      "kern 6" \override #'(kern . 6) \bar-line "|."
      "hair-thickness 6" \override #'(hair-thickness . 6) \bar-line "|."
      "thick-thickness 1.9" \override #'(thick-thickness . 1.9) \bar-line "|."
    }
  }
}
