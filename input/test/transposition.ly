%{
Hi all,

I have extend Mats' patches to allow accidental transposition:
        \keysignature bes es fis;
		= \transpose d =>
        \keysignature gis;

As you can see in output =  the example file "test.ly" there are a few problems left:
- key undo (already in the Todo)
- "wrong" transposition: e.g. \transpose d of fis-major = gis-major (better as-major?).
	The solution gis=as etc. would concern both note and key transposistion

Eric

- Note that transpose eses of fis-major = as-major 
- Note also that key signatures specified with \property keySignature 
  are not transposed!

  /Mats B, March 21, 2001
%}

\header{
title = 	 "Test it";
description = 	 "Transposition Test file";
enteredby = 	 "Eric Bullinger";
copyright = 	 "public domain";
}




vOne =  \notes \relative c''{
        \clef"violin";
        \key d \major;
        \time 2/4 ;
        d4 d |       
        \key fis \major;
        fis4 fis |         
        \key es \major;
        c4 c |
}

vTwo =  \notes \transpose d' { \vOne }

vThree =  \notes \relative c''{
        \clef"violin";
%        \keysignature fis cis;
	\property Staff.keySignature = #'((0 . 1)(3 . 1))
        \time 2/4 ;
        d4 d |       
%        \keysignature bes es fis;
	\property Staff.keySignature = #'((3 . 1)(2 . -1)(6 . -1))
        fis4 fis |         
%        \keysignature fis cis gis dis ais eis;
	\property Staff.keySignature = #'((2 . -1)(5 . -1)(1 . 1)(4 . 1)(0 . 1)(3 . 1))
        cis4 ais4 |
}

vFour =  \notes \transpose d' \vThree

\score {
  \context StaffGroup <
         \context Staff=vOne \vOne
         \context Staff=vTwo \vTwo
         \context Staff=vThree \vThree
         \context Staff=vFour \vFour
  >
        \paper { linewidth= 130.\mm; }
}



