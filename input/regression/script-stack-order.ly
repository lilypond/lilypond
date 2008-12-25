
\version "2.12.0"
\header {
  texidoc = "Scripts can be stacked. The order is determined by a
priority field, but when objects have the same priority, the input
order determines the order. Objects specified first are closest to the note.
"
}

\layout { ragged-right = ##t}

\relative c'' { c4^"inner up"^"outer up"_"inner down"_"outer down" }



