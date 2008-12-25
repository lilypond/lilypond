\version "2.12.0"
\header {
  texidoc = "Contexts of the same type can be nested."
}

\new StaffGroup \relative c' <<
  \new Staff { c1 }
  \new StaffGroup <<
    \new Staff { c1 }
    \new StaffGroup <<
      \new Staff { c1 }
      \new Staff { c1 }
    >>
  >>
>>
