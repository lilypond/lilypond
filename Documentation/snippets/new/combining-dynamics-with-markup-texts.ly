\version "2.24.0"

\header {
  categories = "Expressive marks, Really simple, Text"

  texidoc = "
Some dynamics may involve text indications (such as @qq{più @emph{f}} or
@qq{@emph{p} subito}). These can be produced using a @code{\\markup}
block; the resulting object behaves like a @code{TextScript} grob.

See also @qq{Combining dynamics with markup texts (2)}.
"

  doctitle = "Combining dynamics with markup texts"
} % begin verbatim


piuF = \markup { \italic più \dynamic f }

\score {
  \relative c'' {
    c2\f c-\piuF
  }
}
