% Toplevel AsciiScript initialisation file. 

\version "1.3.59";

\include "declarations-as.ly"

% burp.  need to override lily.scm
#(define cmr-alist 
  '(("bold" . "cmbx") 
    ("brace" . "as-braces")
    ("dynamic" . "cmr") 
    ("default" . "as-dummy") 
    ("feta" . "feta") 
    ("feta-1" . "feta") 
    ("feta-2" . "feta") 
    ("finger" . "as-number") 
    ("typewriter" . "cmtt") 
    ("italic" . "cmti") 
    ("roman" . "cmr") 
    ("script" . "cmr") 
    ("large" . "cmbx") 
    ("Large" . "cmbx") 
    ("mark" . "as-number") 
    ("number" . "as-number") 
    ("volta" . "as-number"))
)

\paper {
    \paper_as_nine
    \translator { \StaffContext barSize = \staffheight; }

    % no beam-slope
    %\translator { \VoiceContext beamHeight = #0; }
}  

\maininput

