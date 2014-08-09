\version "2.19.11"

\header {
  texidoc = "All text-interface grobs should have
@w{@code{baseline-skip}} and @w{@code{word-space}} values scaled
along with notation size when using the @code{\magnifyMusic}
command."
}

music = {
  b'1^\markup {
    \center-column { a c }
    \center-column { b d }
  }
}

{
  \override TextScript.baseline-skip = #2
  \magnifyMusic 0.5 \music
  \music
  \magnifyMusic 2.0 \music
}
