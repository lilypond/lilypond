
\version "2.3.22"
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


