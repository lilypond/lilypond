
\header {

texidoc = "When ledgered notes are very close, for example, in grace
notes, they are kept at a minimum distance to prevent the ledgers from
disappearing."

}

\version "2.11.51"
\paper { ragged-right = ##t}
\relative c'' {
  \time 2/4 
  c8 c c c
  c \grace { \stemDown e'32[ c a f] } f8 f f 
}
