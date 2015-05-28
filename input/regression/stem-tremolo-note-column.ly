\version "2.19.21"
\header{
  texidoc="Stem tremolos count in a note column's horizontal skyline.
"
}


\relative {
  \autoBeamOff
  \override NoteHead.stencil = #(ly:make-stencil '() '(0 . 0) '(0 . 0))
  \repeat unfold 8 { b'8:32 }
}
