\version "2.19.21"
\header {

  texidoc = "In 4/4 time, the first and second and third and fourth
beats should be beamed together if only eighth notes are involved.
If any shorter notes are included, each beat should be beamed separately."

}
\layout { ragged-right = ##t }

\relative {
  \repeat unfold 8 { a'8} |
  a8 a a a16 a a8 a  a16 a a8 |
  r16 a8. a8 a16 a r8. a16 a8 a16 a |
}
