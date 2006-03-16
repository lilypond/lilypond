
\version "2.7.39"
\header { texidoc = "The magnification can be set for any font. Note
that this does not change variable symbols such as beams or slurs. 
" }


\relative c'' \context Voice {
  \override NoteHead  #'font-magnification = #0.9
  c4
  \override NoteHead  #'font-magnification = #0.8


  c4-"normal"
  %% why doesn't this  work?
  c4-\markup \bold \magnify #2.0 "foobar"

  \override NoteHead  #'font-magnification = #1.2
  \override TextScript  #'font-magnification = #2.0
  c4-"big"
  \override NoteHead  #'font-magnification = #1.6
  c4
}




