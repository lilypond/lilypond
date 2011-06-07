\version "2.14.0"

\header { texidoc = "Segno bar lines can be used to mark
                     the begin and the end of a segno part."
        }

\paper { ragged-right = ##t }

\relative \new StaffGroup <<
  \new Staff {
    c4 \bar "S" c \bar "|S" c \bar "S|" c \bar ":|S" \break
    c4 c \bar ":|S|:" c c \bar ":|S.|:" \break
    c4 c c2 \bar ".S|:" \break
    c1 \bar ":|S." \break
    c1 \bar "S" \break
    c1 }
  \new Staff {
    c4 c c c
    c4 c c c
    c4 c c c
    c4 c c c
    c4 c c c
    c1
  }
>>
