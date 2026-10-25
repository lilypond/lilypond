\version "2.27.5"

\header { texidoc = "For maintaining several layers of table of contents at the
same time, @code{\\newTocItem} can create variants of @code{\\tocItem} that are
frozen to serve a particular table of contents rather than the most recent
one."}

\layout { ragged-right = ##t }

\book {
  \header { title = "Music" ragged-bottom = ##t tagline = ##f }
  \setupTableOfContents
  topTocItem = \newTocItem \default
  \markuplist \table-of-contents
  \bookpart {
    \paper { }
    \header { title = "Rhythms" }
    \setupTableOfContents
    \topTocItem rhythms #"Rhythms"
    \markuplist \table-of-contents
    \tocItem #"Straight rhythm"
    \topTocItem rhythms.straight #"straight"
    \score { \header { title = "Straight rhythm" } { 4 4 4 4 } }
    \tocItem #"Dotted rhythm"
    \topTocItem rhythms.dotted #"dotted"
    \score { \header { title = "Dotted rhythm" } { 4. 8 4. 8 } }
  }
  \bookpart {
    \paper { }
    \header { title = "Pitches" }
    \topTocItem pitches #"Pitches"
    \topTocItem pitches.single #"single"
    { c'1 }
    \topTocItem pitches.double #"double"
    { <c' g'>1 }
  }
}
