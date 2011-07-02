\version "2.15.6"

\header {
  texidoc = "@code{\\quoteDuring} and @code{\\cueDuring} shall properly quote
voices that create a sub-voice. The sub-voice will not be quoted, though.
"
}


quoteMe = \relative c' {
  c4 c
  \new Voice {
    c4 c
  }
}
\addQuote quoteMe \quoteMe

\relative c'' {
  c4 \cueDuring #"quoteMe" #DOWN { r4 } % <- show a cue note from quoteMe
  c4 \cueDuring #"quoteMe" #DOWN { r4 } % <- no cue note due to sub-voice
}
