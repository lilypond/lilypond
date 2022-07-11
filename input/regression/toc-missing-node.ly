\version "2.23.11"

\header {
  texidoc = "A missing node in a structured TOC is handled gracefully."
}

#(ly:set-option 'warning-as-error)
#(ly:expect-warning (G_ "TOC node ~a not defined") 'foo)
#(ly:expect-warning (G_ "TOC node ~a not defined") 'eggs)

\markuplist \table-of-contents

\tocItem foo.bar "oops" %% \tocItem foo should have been defined

\tocItem spam "spam"
\tocItem spam.eggs.baz "oops" %% \tocItem spam.eggs should have been defined

{ c' }
