\header {
texidoc="Trend of broken slur with user-overridden stem attachment"
}
\score {
  \notes\relative c' {
    \property Voice.Slur \override #'attachment = #'(stem . stem)
    f( c' c c \break
    c c c \stemUp ) c
  }
  \paper {
    linewidth=40*\staffspace
  }
}
