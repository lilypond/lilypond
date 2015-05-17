\version "2.19.21"

\header {
  texidoc = "
@code{\\laissezVibrer} ties should also work on individual notes of a chord.
" }

\relative {
  <d'-\laissezVibrer g>1
  <d^\laissezVibrer g_\laissezVibrer>1
}
