\version "2.19.21"

\header { texidoc = "
Whole notes in a monochord must be properly offset so that the heads just
touch each other.  On the other hand, a stem should touch both notes.
" }

\relative {
  \time 4/1
  <a' a>\longa \breve 1 2 4 8 8 \break
  <c c>\longa \breve 1 2 4 8 8
}

\paper {
  ragged-right = ##t
}
