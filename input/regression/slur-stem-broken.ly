\version "1.5.68"
\header {
texidoc="Trend of broken slur with user-overridden stem attachment should also
follow the same vertical direction it would have had in unbroken state."
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
