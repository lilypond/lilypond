
\version "2.4.0"
\header {
    texidoc= "Text spanners should not repeat start text when broken."
}
\layout {
    raggedright = ##t 
}

\relative c'' {
    \set crescendoText = #"cresc."
    \set crescendoSpanner = #'dashed-line
    c1\< c \break
    c1 c\! \break
}

