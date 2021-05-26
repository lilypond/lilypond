\version "2.23.3"

\header {
  texidoc = "Cyclic dependencies are detected and warned about.  When
the @code{debug-property-callbacks} option is set, a backtrace is
printed with the warning."
}

#(ly:set-option 'debug-property-callbacks)

{
  \override NoteHead.color =
    #(lambda (grob)
       (ly:grob-property grob 'layer))
  \override NoteHead.layer =
    #(lambda (grob)
       (ly:grob-property grob 'stencil))
  \override NoteHead.stencil =
    #(lambda (grob)
       (ly:grob-property grob 'color))
  c
}
