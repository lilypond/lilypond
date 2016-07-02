\header { texidoc = "Deeply nested system braces, brackets, etc., may be
  created with the @code{systemStartDelimiterHierarchy} property."
}

\version "2.19.21"

\paper {
  ragged-right = ##t
}

\new StaffGroup
\relative <<
  \set StaffGroup.systemStartDelimiterHierarchy
    = #'(SystemStartSquare (SystemStartBracket a (SystemStartSquare b c)) d)
  \new Staff { c'1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  >>
