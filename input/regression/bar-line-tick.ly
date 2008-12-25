
\header { texidoc = "A ticked bar line is a short line of the same length as a
  staff space, centered on the top-most barline." }

\version "2.12.0"

\paper {  ragged-right = ##t }

\relative \new StaffGroup <<
  \new Staff {
    c4 \bar "'" c }
  \new Staff {
     c c
  }
>>

