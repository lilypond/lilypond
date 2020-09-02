\version "2.21.6"

\header{
  texidoc="This test covers a volta repeat as top-level music with the
repeat body being simultaneous music."
}

\score {
  %% formerly, the repeat bar was missing (Issue #6022)
  \repeat volta 2 << d'1 >>
}
