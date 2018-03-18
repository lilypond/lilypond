\version "2.21.0"

\header {
  texidoc = "@code{\\etc} can be used for constructing
  event functions for @samp{TextScript} events with sequences
  starting with @samp{-}, @samp{^}, or @samp{_}.  This example should
  have notes all adorned in the same manner."
}

\layout { ragged-right = ##t }

etcI = ^\markup \bold \with-color #red \etc
etcII = _\etc
etcIII = ^\tweak color #red \etc
etcIV = _\tweak color #green -\markup \italic \etc

{
  c'1\etcI "up" \etcII \markup \italic \with-color #green "down"
  c'1\etcIII \tweak font-series #'bold -"up" \etcIV "down"
}
