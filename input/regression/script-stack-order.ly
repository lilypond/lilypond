
\version "2.19.21"
\header {
  texidoc = "Scripts can be stacked. The order is determined by a
priority field, but when objects have the same priority, the input
order determines the order. Objects specified first are closest to the note.
"
}

\layout { ragged-right = ##t}

\relative {
  c''4^"up 1"^"up 2"^"up 3"_"down 1"_"down 2"_"down 3" c c c
  c4^"1"^"2"\turn  c\turn ^"1"^"2" c c
}
