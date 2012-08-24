
\version "2.16.0"

\header
{
  texidoc="A knee is made automatically when a horizontal
beam fits in a gap between note heads that is larger than a predefined
threshold.
"
}
\layout{ ragged-right = ##t }

\context Staff \relative c''{ 
  c'8[ c,,]  c8[ e']
  c,16[ e g c e g c c,,] 
}
