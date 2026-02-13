\version "2.25.34"

\header {
  categories = "Expressive marks, Vocal music"

  texidoc = "
The @code{\\nonArpeggiato} command can be used to indicate the
division of voices where there are no stems to provide the information.
This is often seen in choral music.
"

  doctitle = "Using a bracket to clarify divisi"
}


\include "english.ly"

\score {
  \relative c'' {
    \key a \major
    \time 2/2
    <<
      \new Voice = "upper" <<
        {
          \voiceOne
          a2( b2
          <b d>1\nonArpeggiato)
          <cs e>\nonArpeggiato ~
          <cs e>4
          \fine
        }
        \addlyrics { \lyricmode { A -- men. } }
      >>
      \new Voice = "lower" {
        \voiceTwo
        a1 ~
        a
        a ~
        a4
        \fine
      }
    >>
  }
}
