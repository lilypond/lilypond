
\version "2.3.17"

\header {
    
    texidoc = "There a many types of bar lines available."

}

\paper { raggedright = ##t }

\relative {
    \override Score.RehearsalMark #'font-family = #'typewriter
    c4 \bar "|" \mark \markup {  \simple #"\"|\"" } 
    c \bar "|:" \mark \markup {  \simple #"\"|:\"" } 
    c \bar "||" \mark \markup {  \simple #"\"||\"" } 
    \bar ":|" \mark \markup {  \simple #"\":|\"" } 
    c \bar ".|" \mark \markup {  \simple #"\".|\"" } 
    c \bar ".|." \mark \markup {  \simple #"\".|.\"" } 
    c \bar ":|:" \mark \markup {  \simple #"\":|:\"" } 
    c \bar "|." \mark \markup {  \simple #"\"|.\"" } 
    c \bar ":" \mark \markup {  \simple #"\":\"" } 
}
