
\version "1.9.6"

\header{ texidoc = "@cindex Transposition
Transposition test file."
}


%{
Hi all,

I have extend Mats' patches to allow accidental transposition:
        \keysignature bes es fis
		= \transpose c' d =>>
        \keysignature gis

As you can see in output =  the example file "test.ly" there are a few problems left:
- key undo (already in the Todo)
- "wrong" transposition: e.g. \transpose c' d of fis-major = gis-major (better as-major?).
	The solution gis=as etc. would concern both note and key transposistion

Eric

- Note that transpose eses of fis-major = as-major 
- Note also that key signatures specified with \property keySignature 
  are not transposed!

  /Mats B, March 21, 2001
%}




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
%        \keysignature fis cis
	\property Staff.keySignature = #'((0 . 2)(3 . 2))
        \time 2/4 
        d4 d |       
%        \keysignature bes es fis
	\property Staff.keySignature = #'((3 . 2)(2 . -2)(6 . -2))
        fis4 fis |         
%        \keysignature fis cis gis dis ais eis
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




