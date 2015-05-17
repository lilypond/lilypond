
\header {

texidoc = "When ledgered notes are very close, for example, in grace
notes, they are kept at a minimum distance to prevent the ledgers from
disappearing."

}

\version "2.19.21"
\paper { ragged-right = ##t}
\relative {
  \time 2/4 
  c''8 c c c
  c \grace { \stemDown e'32[ c a f] } f8 f f 
}
