\version "2.17.15"

\header { texidoc = "New bar line styles can be defined by @code{\\defineBarLine}."
        }

\paper { ragged-right = ##t }

\defineBarLine "[|;" #'("|" "[|;" " |")
\defineBarLine ";|]" #'(";|]" "" " |")

\relative c' \new StaffGroup <<
  \new Staff {
    c4 c \bar "[|;" c c \bar ";|]" \break
    c4 c \bar ";|]" c c \bar "[|;" \break
    c1 }
  \new Staff {
    c4 c c c
    c4 c c c
    c1
  }
>>
