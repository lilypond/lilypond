
\version "2.19.21"
\header {
    texidoc = "Beaming can be also given explicitly."
}

\layout { ragged-right= ##t }

\relative
{
  c'16[ c8.]
  c8.[ c16]
  c8[ c16 c16 c8]
  c8[ c16 e16 g8]

}
