% Toplevel initialisation file. 

#(define-public point-and-click #f)
#(define-public midi-debug #f)

#(reset-drum-kit)

\version "1.9.0"

\include "declarations-init.ly"

#(ly:set-option 'old-relative #t)
#(ly:set-point-and-click #f)

\maininput

#(if (and (ly:get-option 'old-relative)
      (not (ly:get-option 'old-relative-used)))
  (ly:warn "Old relative compatibility was not used."))
