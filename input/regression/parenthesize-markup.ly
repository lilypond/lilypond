\version "2.21.0"

\header{

  texidoc="
The parenthesize markup will place parentheses
around any stencil.

The angularity of the parentheses can be adjusted.
"

}

\score {
  \relative {c''^\markup {
    \parenthesize {
      \column { "A" "B" "C" }
    }
  } c c c
  c^\markup
    \override #'((angularity . 2) (width . 0.5))
    \parenthesize
    \column { "A" "B" "C" }
  c c c
  }
}
