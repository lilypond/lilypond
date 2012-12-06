\version "2.17.5"

\header {
  texidoc = "Volta bracket end hooks can be added for other bar line types.
"
}

#(allow-volta-hook "|")

\new Staff {
  \repeat volta 2 { c'1 }
  \alternative { { c'1 } { d'1 } }
  e'1
}