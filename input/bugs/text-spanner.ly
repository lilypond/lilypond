
% text spanners should not repeat start text when broken

\score {
  \notes \relative c'' {
    \property Voice.crescendoText = #"cresc."
    \property Voice.crescendoSpanner = #'dashed-line
    c1\< c \break
    c1 \!c \break
    }
  \paper {
    linewidth = 40*\staffspace;
  }
}
