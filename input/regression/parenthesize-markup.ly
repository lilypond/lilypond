\version "2.13.13"

\header{

  texidoc="
The parenthesize markup will place parentheses
around any stencil.

The angularity of the parentheses can be adjusted.
"

}

\score {
  c''^\markup {
    \parenthesize {
      \column { "A" "B" "C" }
    }
  }
}

