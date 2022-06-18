\version "2.23.10"

\header { texidoc = "New bar line styles can be defined by @code{\\defineBarLine}."
        }

\paper { ragged-right = ##t }

\defineBarLine "[|;" #'("|" #t " |")
%% Placing a zero-width bar line at the beginning of a line is
%% unusual, but it's good to test it somewhere.  (Don't copy this.)
\defineBarLine ";|]" #'(#t "" " |")

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
