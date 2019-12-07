
\version "2.19.21"
\header {
    texidoc= "Text spanners should not repeat start text when broken."
}
\layout {
    ragged-right = ##t 
}

\relative {
    \set crescendoText = "cresc."
    \set crescendoSpanner = #'text
    c''1\< c \break
    c1 c\! \break
}

