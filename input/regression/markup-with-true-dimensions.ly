\version "2.23.9"

\header {
  texidoc = "The markup commands @code{\\with-true-dimension} and
@code{\\with-true-dimensions} give a markup the extents given
by the stencil's outline."
}

\markup
  \fontsize #10
  \override #'((box-padding . 0) (thickness . 0.02))
  \line
  \box {
    \musicglyph "scripts.trill"
    \with-true-dimension #X \musicglyph "scripts.trill"
    \with-true-dimension #Y \musicglyph "scripts.trill"
    \with-true-dimensions \musicglyph "scripts.trill"
  }
