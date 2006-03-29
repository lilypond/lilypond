
\version "2.7.39"

\header {
    
    texidoc = "There a many types of bar lines available."

}

\layout { ragged-right = ##t }

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
