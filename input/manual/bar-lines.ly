
\version "2.10.0"

\header {
    
    texidoc = "There are many types of bar lines available."

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
    c c c \bar "dashed" \mark \markup { \simple #"dashed" }
    c c c c
    \bar "||:" \mark \markup { \tiny \typewriter "unbroken" \simple
#"||:" }
    c c c c
    \break
    \bar "||:" \mark \markup { \tiny \typewriter "broken" \simple
#"||:" }
    c
}
