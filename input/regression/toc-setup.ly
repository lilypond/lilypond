\version "2.27.5"

\header { texidoc = "@code{\\setupTableOfContents} can be used to establish a
table of contents at the current level even if one would already have been
accessible, or if the first @code{\\tocItem} could lead to creation at an
unwanted level.  This example has two bookparts, with the first bookpart having
its own subordinate table of contents while the second bookpart just uses the
main table of contents." }

\layout { ragged-right = ##t }

\book {
  \header { title = "Music" ragged-bottom = ##t tagline = ##f }
  \setupTableOfContents
  \markuplist \table-of-contents
  \bookpart {
    \paper { }
    \header { title = "Rhythms" }
    \tocItem #"Rhythms"
    \setupTableOfContents
    \markuplist \table-of-contents
    \tocItem #"Straight rhythm"
    \score { \header { title = "Straight rhythm" } { 4 4 4 4 } }
    \tocItem #"Dotted rhythm"
    \score { \header { title = "Dotted rhythm" } { 4. 8 4. 8 } }
  }
  \bookpart {
    \header { title = "Pitches" }
    \tocItem #"Single pitch"
    { c'1 }
    \tocItem #"Double pitch"
    { <c' g'>1 }
  }
}
