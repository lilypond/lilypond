
\header {
  texidoc = "Beams over skips do not cause a segfault."
}

\version "2.14.2"

\layout { ragged-right = ##t }

music = {
  \clef bass r2 r4 r8 f,
  r2 r4 g,8 r
  r4 f, 8 r8 r2
}

beams = {
  \repeat "unfold" 24 { s8[ s ] s[ s]}
}

\new Staff {
  \context Voice << { \beams } { \music}>>
}
