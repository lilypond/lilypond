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
%}

\header{
title =	 "Test it";
description =	 "Transposition Test file";
enteredby =	 "Eric Bullinger";
copyright =	 "public domain";
}


\version "1.0.7";

vOne = \notes \relative c''{
        \clef"violin";
        \key d;
        \time 2/4 ;
        d4 d |       
        \key fis;
        fis4 fis |         
        \key es;
        c4 c |
}

vTwo = \notes \transpose d' { \vOne }

vThree = \notes \relative c''{
        \clef"violin";
        \keysignature fis cis;
        \time 2/4 ;
        d4 d |       
        \keysignature bes es fis;
        fis4 fis |         
        \keysignature fis cis gis dis ais eis;
        cis4 ais4 |
}

vFour = \notes \transpose d' \vThree

\score {
  \type StaffGroup <
         \vOne
         \vTwo
         \vThree
         \vFour
  >
        \paper { linewidth= 130.\mm; }
}



