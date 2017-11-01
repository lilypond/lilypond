\version "2.21.0"

\header {
  texidoc = "
@code{\\repeatTie} ties should also work on individual notes of a chord.
" }

\relative {
  <d'-\repeatTie g>1
  <d^\repeatTie g_\repeatTie>1
  <d g>\repeatTie
}

