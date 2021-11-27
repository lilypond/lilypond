\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="If the body of a segno repeat is empty, the result might be
ugly, but it does not manifestly contradict the input.  The margin
labels show the expected note performance sequence."
}

body = { }

\score {
  \new Staff {
    R1 | \repeat segno 2 \body | R1
  }
}

\score {
  \new Staff \with { instrumentName = "A" } {
    %% This test is not focused on empty alternatives, however an
    %% empty final alternative is the typical way to code "al Coda"
    %% form to avoid getting a coda mark.  The user would normally
    %% define a section label instead, but we want to test without
    %% one.
    R1 | \repeat segno 2 \body \alternative { a'1 <> } | R1
  }
}

\score {
  \new Staff \with { instrumentName = "AB" } {
    R1 | \repeat segno 2 \body \alternative { a'1 b' } | R1
  }
}

\score {
  \new Staff \with { instrumentName = "AAB" } {
    R1 | \repeat segno 3 \body \alternative {
      \volta 1,2 a'1
      \volta 3 b'
    } | R1
  }
}

\score {
  \new Staff \with { instrumentName = "ABB" } {
    R1 | \repeat segno 3 \body \alternative {
      \volta 1 a'1
      \volta 2,3 b'
    } | R1
  }
}

\score {
  \new Staff \with { instrumentName = "AABB" } {
    R1 | \repeat segno 4 \body \alternative {
      \volta 1,2 a'1
      \volta 3,4 b'
    } | R1
  }
}
