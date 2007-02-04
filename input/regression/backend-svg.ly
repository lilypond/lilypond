
#(ly:set-option 'backend 'svg)
#(set! output-count 1)


\include "typography-demo.ly"



#(define outname (ly:parser-output-name parser))

#(ly:set-option 'backend 'eps)

%% TODO: what to do if inkscape fails?
#(display "Invoking inkscape...\n")
#(system (format #f "inkscape -T -E ~a-1.eps ~a-1.svg" outname outname))
#(set! output-count 0)
#(set-default-paper-size "a5")
\book { 
  \header {
    texidoc = "SVG output, rendered through inkscape."
    title = "SVG"
  } 



  \score {
    \lyrics {
      \markup {
	\epsfile #X #30.0 #(format #f "~a-1.eps" outname)
      }
      x x x
    }
  }
}
