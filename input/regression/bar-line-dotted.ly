
\header {
  texidoc = "The dots in a dotted bar line are in spaces."

}

\version "2.12.0"

\paper {  ragged-right = ##t }

\relative \new StaffGroup <<
  \new Staff {
    c4 \bar ":" c }
  \new Staff {
    c c
  }
>>

