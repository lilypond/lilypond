\header {
texidoc="Bar number settable and padding adjustable.";
}

\score {
  \notes \relative c'' {
     c1 c\break
     c1 c\break
     \property Score.currentBarNumber = #25
     \property Score.BarNumber \override #'padding = #3
     c1 c\break
  }
  \paper {
    linewidth = 40*\staffspace;
    \translator {
      \BarNumberingStaffContext
    }
  }
}
