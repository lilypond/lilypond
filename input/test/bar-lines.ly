
\version "2.4.0"

\header {
    
    texidoc = "There a many types of bar lines available."

}

\layout { raggedright = ##t }

\relative {
    \override Score.RehearsalMark #'padding = #3
    
    c4 \bar "|" \mark \markup {  \simple #"|" }
    c \bar "|:" \mark \markup {  \simple #"|:" }
    c \bar "||" \mark \markup {  \simple #"||" }
    c \bar ":|" \mark \markup {  \simple #":|" }
    c \bar ".|" \mark \markup {  \simple #".|" }
    c \bar ".|." \mark \markup {  \simple #".|." }
    c \bar ":|:" \mark \markup {  \simple #":|:" }
    c \bar "|." \mark \markup {  \simple #"|." }
    c \bar ":" \mark \markup {  \simple #":" }
    c
}
