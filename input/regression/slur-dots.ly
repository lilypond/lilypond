
\version "2.6.0"
\header{
texidoc="Slurs should not get confused by augmentation dots.  With a lot
of dots, the problems becomes more visible."
}
\score {
  \relative c'' {
    c4.............( c)
  }
  \layout {
    raggedright = ##t
  }
}


