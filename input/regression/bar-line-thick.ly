
\header { texidoc = "A thick bar line is created by \bar \".\", which is consistent with e.g. \bar \"|.\"" }

\version "2.12.0"

\paper {  ragged-right = ##t }

\relative \new StaffGroup <<
  \new Staff {
    c4 \bar "." c }
  \new Staff {
     c c
  }
>>

