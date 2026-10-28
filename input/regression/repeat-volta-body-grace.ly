\version "2.27.5"

\header{
  texidoc="If the body of a volta repeat is only a grace note, it is still
engraved as expected."
}

#(ly:set-option 'warning-as-error #t)

body = \grace f'8

\new Score {
  \repeat volta 2 \body | b'1
}

\new Score {
  b'1 | \repeat volta 2 \body
}

\new Score {
  b'1 | \repeat volta 2 \body | b'1
}

\new Score {
  \repeat volta 2 \body \alternative { a'1 b' } | R1
}

\new Score {
  R1 | \repeat volta 2 \body \alternative { a'1 b' }
}

\new Score {
  R1 | \repeat volta 2 \body \alternative { a'1 b' } | R1
}
