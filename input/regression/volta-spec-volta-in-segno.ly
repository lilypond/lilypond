\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\volta} and @code{\\unfolded} can remove/add music
in the main body of a repeated section even if they change the length.
In this case, a repeat is skipped after @emph{D.C.}  A volta bracket
appears by default."
}

innerRepeatedSection = s1_"B"
piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1_"A"
    \volta #'() \sectionLabel \markup \small "bracket expected"
    \volta 1 \repeat volta 2 \innerRepeatedSection
    \volta 2 \unfolded \innerRepeatedSection
    s1_"C"
  }
}

\new Score { \piece }
\new Score { \unfoldRepeats \piece }
