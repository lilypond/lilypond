\version "2.23.6"

\header {
  texidoc = "Cyclic dependencies are detected and warned about.  When
the @code{debug-property-callbacks} option is set, a backtrace is
printed with the warning."
}

#(ly:set-option 'debug-property-callbacks)

% No lambdas: Guile 2 prints them with their object address,
% making for nondeterministic output.
#(define (color-callback grob)
   (ly:grob-property grob 'layer))

#(define (layer-callback grob)
   (ly:grob-property grob 'stencil))

#(define (stencil-callback grob)
   (ly:grob-property grob 'layer))

{
  \override NoteHead.color = #color-callback
  \override NoteHead.layer = #layer-callback
  \override NoteHead.stencil = #stencil-callback
  c
}
