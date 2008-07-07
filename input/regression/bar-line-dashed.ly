
\header { texidoc = "The dashes in a dashed bar line covers staff
  lines exactly. Dashed barlines between staves start and end on a
  half dash precisely." }

\version "2.11.51"

\paper {  ragged-right = ##t }

\relative \new StaffGroup <<
  \new Staff {
    c4 \bar "dashed" c }
  \new Staff {
    c c
  }
>>

