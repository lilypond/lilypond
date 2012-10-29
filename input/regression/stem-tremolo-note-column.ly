\version "2.17.6"
\header{
  texidoc="Stem tremolos count in a note column's horizontal skyline.
"
}


\relative c'' {
  \autoBeamOff
  \override NoteHead.stencil = #(ly:make-stencil '() '(0 . 0) '(0 . 0))
  \repeat unfold 8 { b8:32 }
}
