
\version "2.3.4"
\header {
texidoc="Trend of broken slur with user-overridden stem attachment should also
follow the same vertical direction it would have had in unbroken state."
}
\score {
  \relative c' {
    \override Slur  #'attachment = #'(stem . stem)
    f( c' c c \break
    c c c \stemUp  c)
  }
  \paper {
    linewidth=40*\staffspace
  }
}

