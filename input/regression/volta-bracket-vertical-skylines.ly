\version "2.16.0"

\header {
  texidoc = "Volta brackets are vertically fit to objects below them.
"
}

\new Staff {
  \repeat volta 3 { r2 a''''4 r4 }
  \alternative { { r2 d''''4 r4 } { r2 d''''4 r4 } { r2 d''''4 r4 } }
  \repeat volta 3 { r2 a''''4 r4 }
  \alternative { { r2 a''''4 r4 } { r2 a''''4 r4 } { r2 a''''4 r4 } }
}