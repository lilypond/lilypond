\version "1.5.68"

\header{ texidoc="Automatic kneeing. A knee is made when a horizontal
beam fits in a gap between note heads that is larger than a predefined
threshold.
"
}

\score {
  \context Staff \notes\relative c''{ 
    [c'8 c,,] [c8 e']
    [c,16 e g c e g c c,,] 
  }
  \paper{
    linewidth = 40*\staffspace
  }
}
