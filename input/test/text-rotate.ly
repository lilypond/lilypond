\header {
    latexpackages = "graphicx"
    
    texidoc = "
@cindex rotated text
@cindex choir, rotated text

Rotated text may be faked using inline TeX (or inline
postscript). To see the result, this files must be processed with
the lilypond.py script.

"
    

    }
\score {
\new Staff \notes \relative c'' {
    \property Staff.instrument	= #"\\rotatebox{90}{Chor}"

    c4 c4 }

\paper { raggedright = ##t }
}


