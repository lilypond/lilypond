
\version "2.1.28"
\header{
texidoc="Slurs should not get confused by augmentation dots.  With a lot
of dots, the problems becomes more visible."
}
\score {
  \notes\relative c'' {
    c4.............( c)
  }
  \paper {
    raggedright = ##t
  }
}


