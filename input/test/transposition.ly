
\version "2.3.4"

\header{ texidoc = "@cindex Transposition
Transposing has also an effect key signature, if it is given using
@code{\key}. If @code{keySignature} is set explicitly instead, 
the key signature is not transposed."

     }

% should the explicitly set key signature transpose also? -HJJ

vOne =  \relative c''{
        \clef"violin"
        \key d \major
        \time 2/4 
        d4 d |       
        \key fis \major
        fis4 fis |         
        \key es \major
        c4 c |
}

vTwo =  \transpose c d { \vOne }

vThree =  \relative c''{
        \clef"violin"
%        keysignature fis cis
	\set Staff.keySignature = #'((0 . 2)(3 . 2))
        \time 2/4 
        d4 d |       
%        keysignature bes es fis
	\set Staff.keySignature = #'((3 . 2)(2 . -2)(6 . -2))
        fis4 fis |         
%        keysignature fis cis gis dis ais eis
	\set Staff.keySignature = #'((2 . -2)(5 . -2)(1 . 2)(4 . 2)(0 . 2)(3 . 2))
        cis4 ais4 |
}


vFour =  \transpose c d \vThree

\score {
  \context StaffGroup <<
         \new Staff \vOne
         \new Staff \vTwo
         \new Staff \vThree
         \new Staff \vFour
  >>
        \paper { linewidth= 130.\mm raggedright = ##t }
}




