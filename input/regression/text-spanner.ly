
\version "2.1.22"
\header {
texidoc= "Text spanners should not repeat start text when broken."
}

\score {
  \notes \relative c'' {
    \set crescendoText =  #"cresc."
    \set crescendoSpanner =  #'dashed-line
    c1\< c \break
    c1 c\! \break
    }
  \paper {
    linewidth = 40*\staffspace
  }
}

