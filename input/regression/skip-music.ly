\version "2.23.6"

\header {
  texidoc = "@code{\\skip} can skip over music.  The expected output
is two A notes separated by two empty measures."
}

skippedMusic = {
  c1)^"all ignored" |
  e1~ \mark \default |
  \fine
}

\fixed c' {
  a1 | \skip \skippedMusic | a1
}
