
\version "1.9.6"

\header{ texidoc = "@cindex Transposition
Transposition test file."
}



vOne =  \notes \relative c''{
        \clef"violin"
        \key d \major
        \time 2/4 
        d4 d |       
        \key fis \major
        fis4 fis |         
        \key es \major
        c4 c |
}

vTwo =  \notes \transpose c d { \vOne }

vThree =  \notes \relative c''{
        \clef"violin"
%        keysignature fis cis
	\property Staff.keySignature = #'((0 . 2)(3 . 2))
        \time 2/4 
        d4 d |       
%        keysignature bes es fis
	\property Staff.keySignature = #'((3 . 2)(2 . -2)(6 . -2))
        fis4 fis |         
%        keysignature fis cis gis dis ais eis
	\property Staff.keySignature = #'((2 . -2)(5 . -2)(1 . 2)(4 . 2)(0 . 2)(3 . 2))
        cis4 ais4 |
}


vFour =  \notes \transpose c d \vThree

\score {
  \context StaffGroup <<
         \new Staff \vOne
         \new Staff \vTwo
         \new Staff \vThree
         \new Staff \vFour
  >>
        \paper { linewidth= 130.\mm raggedright = ##t }
}




