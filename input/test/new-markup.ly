\version "1.7.7"
\header {
    texidoc =" New markup scheme. Semantically more sane. We haven't
    invented a nice syntax yet."
}

\score {
  \notes \context Voice {
     \property Voice.TextScript \set #'molecule-callback = #brew-new-markup-molecule
     c4^#`(,simple-markup "foo")
     c4^#`(,column-markup (
			   (,bold-markup (,simple-markup "foo"))
			   (,simple-markup "bar")))
     }
}
