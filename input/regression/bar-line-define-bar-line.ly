\version "2.19.21"

\header { texidoc = "New bar line styles can be defined by @code{\\defineBarLine}."
        }

\paper { ragged-right = ##t }

\defineBarLine "[|;" #'("|" "[|;" " |")
\defineBarLine ";|]" #'(";|]" "" " |")

\relative \new StaffGroup <<
  \new Staff {
    c'4 c \bar "[|;" c c \bar ";|]" \break
    c4 c \bar ";|]" c c \bar "[|;" \break
    c1 }
  \new Staff {
    c4 c c c
    c4 c c c
    c1
  }
>>
