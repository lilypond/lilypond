
#(ly:set-option 'backend 'svg)
#(set! output-count 1)


\include "typography-demo.ly"

\version "2.11.16"

#(define outname (ly:parser-output-name parser))

#(ly:set-option 'backend 'eps)

%% TODO: what to do if inkscape fails?
#(ly:progress "Invoking inkscape...\n")

%% LD_LIBRARY_PATH is necesssary, otherwise, it doesn't build in GUB.
%% LD_LIBRARY_PATH is part of the start-environment but should be switched off
%% for external inkscape.
#(ly:system (format #f "LD_LIBRARY_PATH= inkscape -T -E ~a-inkscape.eps ~a-1.svg" outname outname)
  (cons
   (format #f "FONTCONFIG_FILE=~a/fonts/fonts.conf" (ly:effective-prefix))
   (ly:start-environment)))

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
	\epsfile #X #30.0 #(format #f "~a-inkscape.eps" outname)
      }
      bla bla bla
    }
  }
}
