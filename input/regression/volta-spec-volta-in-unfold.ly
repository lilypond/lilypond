\version "2.23.0"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\volta} pertains to the innermost repeat.  In this case,
alternative notes are inside a volta repeat, so they are engraved as
chords even though the volta repeat is inside an unfolded repeat."
}

\new Voice \fixed c' {
  \repeat unfold 2 {
    \repeat volta 3 {
      <<
        \volta 1,2 d1
        \volta 3 f
      >>
    } \alternative {
      <<
        \volta 1 f1
        \volta 2 a
      >>
      <d a>
    }
  }
}
