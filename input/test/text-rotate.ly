\header {
    latexpackages = "graphicx"
    
    texidoc = "
@cindex rotated text
@cindex choir, rotated text

Rotated text may be faked using inline TeX (or inline
postscript). To see the result, this files must be processed with
the lilypond.py script, and a line must be uncommented.

"

    %% fixme.
    

    }

\version "2.1.26"

\score {
\new Staff \notes \relative c'' {
    \set Staff.instrument = #"rotatebox{90}{Chor}"

%%  uncomment this for rotation
%    \set Staff.instrument = #"\\rotatebox{90}{Chor}"

    c4 c4 }

\paper { raggedright = ##t }
}


