
\version "1.9.4"
\header {texidoc = "Tests tuplet rests. "}

\score {
  \context Voice \notes \relative c'' {
    \time 2/4
    \times 2/3 { r c,,, c''' }
    \times 2/3 { r c c }
    \times 2/3 { r c r}
    \times 2/3 { r r r}
    \times 2/3 { r c e}
    \times 2/3 { c r e}
    \times 2/3 { r c g}
    \times 2/3 { c r g}
  }
}


