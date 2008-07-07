
\version "2.11.51"
\header {
    texidoc= "Text spanners should not repeat start text when broken."
}
\layout {
    ragged-right = ##t 
}

\relative c'' {
    \set crescendoText = #"cresc."
    \set crescendoSpanner = #'text
    c1\< c \break
    c1 c\! \break
}

