
\version "2.3.17"
\header {
    texidoc= "Text spanners should not repeat start text when broken."
}
\paper {
    raggedright = ##t 
}

\relative c'' {
    \set crescendoText = #"cresc."
    \set crescendoSpanner = #'dashed-line
    c1\< c \break
    c1 c\! \break
}

