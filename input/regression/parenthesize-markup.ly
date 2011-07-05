\version "2.14.0"

\header{

  texidoc="
The parenthesize markup will place parentheses
around any stencil.

The angularity of the parentheses can be adjusted.
"

}

\score {
  \relative c'' {c^\markup {
    \parenthesize {
      \column { "A" "B" "C" }
    }
  } c c c
  c^\markup {
    \override #'(angularity . 2) {
      \override #'(width . 0.5) {
        \parenthesize {
          \column { "A" "B" "C" }
        }
      }
    }
  } c c c
  }
}

