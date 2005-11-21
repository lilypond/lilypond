\header { texidoc = "Deeply nested system braces/brackets/etc. may be created with the
  @code{Nested_system_start_delimiter_engraver}"

}

\version "2.7.18"

\paper {
  raggedright = ##t
}

\new StaffGroup \with {
  \remove "System_start_delimiter_engraver"
  \consists "Nested_system_start_delimiter_engraver"
}
\relative <<
  \set StaffGroup.systemStartDelimiters =
   #'(SystemStartSquare SystemStartBracket SystemStartSquare)
  
  \set StaffGroup.systemStartDelimiterHierarchy = #'((a (b)) c)
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  \new Staff { c1 }
  >>
