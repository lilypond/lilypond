
\version "2.8.0"
\header {
    texidoc= "Text spanners should not repeat start text when broken."
}
\layout {
    ragged-right = ##t 
}

\relative c'' {
    \set crescendoText = #"cresc."
    \set crescendoSpanner = #'dashed-line
    c1\< c \break
    c1 c\! \break
}

