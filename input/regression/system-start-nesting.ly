\header { texidoc = "Deeply nested system braces/brackets/etc. may be created with the
  @code{Nested_system_start_delimiter_engraver}"

}

\version "2.11.51"

\paper {
  ragged-right = ##t
}

\new StaffGroup
\relative <<
  \set StaffGroup.systemStartDelimiterHierarchy
    = #'(SystemStartSquare (SystemStartBracket a (SystemStartSquare b)) d)
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  >>
