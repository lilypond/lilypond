
\version "2.19.21"
\header {

  texidoc = "In combination with a beam, the bracket of the tuplet
bracket is removed. This only happens if there is one beam, as long as
the bracket."

}
\layout { ragged-right= ##t }

\context Voice\relative {
  \tuplet 3/2 { r  c''8[ c8] }
  \tuplet 3/2 {  c8[ c c] }
  \tuplet 3/2 {  c16[ c16]  c8[ c8] }
}

