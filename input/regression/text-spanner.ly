
\version "2.3.4"
\header {
texidoc= "Text spanners should not repeat start text when broken."
}

\score {
   \relative c'' {
    \set crescendoText = #"cresc."
    \set crescendoSpanner = #'dashed-line
    c1\< c \break
    c1 c\! \break
    }
  \paper {
    linewidth = 40*\staffspace
  }
}

