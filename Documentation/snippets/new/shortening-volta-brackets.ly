\version "2.25.23"

\header {
  lsrtags = "repeats"

  texidoc = "
By default, the volta brackets will be drawn over all of the
alternative music, but it is possible to shorten them by
overriding @code{VoltaBracket.musical-length}.  In the next
example, the bracket only lasts one measure, which is a
duration of 3/4.
"

  doctitle = "Shortening volta brackets"
}


\fixed c'' {
  \time 3/4
  c4 c c
  \repeat volta 5 {
    d4 d d
    \alternative {
      \volta 1,2,3,4 {
        \once \override Score.VoltaBracket.musical-length =
        \musicLength 2.
        e4 e e
        f4 f f
      }
      \volta 5 {
        g4 g g
        g2.
      }
    }
  }
}
