\version "2.19.21"
\header {
  texidoc = "Contexts of the same type can be nested."
}

\new StaffGroup \relative <<
  \new Staff { c'1 }
  \new StaffGroup <<
    \new Staff { c1 }
    \new StaffGroup <<
      \new Staff { c1 }
      \new Staff { c1 }
    >>
  >>
>>
