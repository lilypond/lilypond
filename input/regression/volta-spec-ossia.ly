\version "2.23.12"

\header { % regression test for issue 6402
  texidoc="A new context inside @code{\\volta} ends at the proper
time.  The staff with an A note should have only one measure."
}

\new Staff {
  \repeat volta 2 \alternative {
    \volta 1,2 \new Staff a'1
  }
  b'1
}
