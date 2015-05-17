\version "2.19.21"

\header {
  texidoc = "
When a harmonic note is tied in tablature, neither the fret number
nor the harmonic brackets for the second note appear in the tablature.
"
}

music =  \relative {
  s2.  <d'\4\harmonic>4 ~ |
  <d\4\harmonic>1  |
}

\new StaffGroup <<
  \new Staff  { \clef "G_8" \music }
  \new TabStaff  { \clef "moderntab" \music }
>>
