
\version "2.3.17"

\header{ texidoc="A knee is made automatically when a horizontal
beam fits in a gap between note heads that is larger than a predefined
threshold.
"
}

\score {
  \context Staff \relative c''{ 
     c'8[ c,,]  c8[ e']
     c,16[ e g c e g c c,,] 
  }
  \paper{
    linewidth = 40*\staffspace
  }
}
