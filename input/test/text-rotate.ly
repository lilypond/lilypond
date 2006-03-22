\header {
    latexpackages = "graphicx"
    
    texidoc = "
@cindex rotated text
@cindex choir, rotated text

Inline TeX (or PostScript) may be used, for example, to rotate text.
To see the result, use the @code{lilypond.py} script to generate the 
output for printing of the source of this example (commenting one line).

"

    %% fixme.
    

    }

\version "2.8.0"

\score {
\new Staff  \relative c'' {
    \set Staff.instrument = #"rotatebox{90}{Chor}"

%%  uncomment this for rotation
%    \set Staff.instrument = #"\\rotatebox{90}{Chor}"

    c4 c4 }

\layout { ragged-right = ##t }
}


