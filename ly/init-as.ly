% Toplevel AsciiScript initialisation file. 

\version "1.3.96";

\include "declarations-as.ly"

% burp.  need to override lily.scm
#(define cmr-alist
  '(("bold" . "as-dummy") 
    ("brace" . "as-braces")
    ("dynamic" . "as-dummy") 
    ("default" . "as-dummy") 
    ("feta" . "feta") 
    ("feta-1" . "feta") 
    ("feta-2" . "feta") 
    ("finger" . "as-number") 
    ("typewriter" . "as-dummy") 
    ("italic" . "as-dummy") 
    ("roman" . "as-dummy") 
    ("script" . "as-dummy") 
    ("large" . "as-dummy") 
    ("Large" . "as-dummy") 
    ("mark" . "as-number") 
    ("number" . "as-number") 
    ("timesig" . "as-number")
    ("volta" . "as-number"))
)

\paper {
    \paper_as_nine
    \translator { \StaffContext barSize = \staffheight; }

    % no beam-slope
    %\translator { \VoiceContext beamHeight = #0; }
}  

\maininput

