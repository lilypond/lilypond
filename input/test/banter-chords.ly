\version "1.3.42";

\header{
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "Banter named chords";
}

% test German (Banter) naming
% for more conventional naming, comment scm stuff out

% urg, 
% this shows a serious shortcoming with our guile approach:
% we can't (easily) display banter/non banter chords alongside
% eachother.  These guile lists are fixed in the guile environment
% when this file has been parsed...

% {
#(set! note-names-alist
      (append 
      '(
	; use these for German naming
	((6 . 0) . ("H" ""))
	((6 . -1) . ("B" ("feta-1" . "")))
	)
      note-names-alist))

#(set! chord-names-alist
      (append 
      '(
        (((0 . 0) (2 . -1) (4 . -1)) . ("m" . ("script" . "5-")))
	; Co iso Cm5-7-
	; urg, niet te pruimen
        ; (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" . ("feta-1" . ".")))
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("" . ("script" . "o")))
	)))
% }

chord = \notes\transpose c''\chords{
	% dim modifier means: lower all implicit additions
	c:dim9
	c:dim
	c:dim7
	% explicit additions are taken as entered:
	c:m5-.7-
	% note that 7 is a special case: it's always lowered by 1...
	c:dim7-.9
	c:dim9-.11

	% test German names
	b:dim7
	bes:m5-

	\break

	c:sus2  %?
	c:sus4
	c^3
	c^3.5
	c:2.6^5
	c:dim^5-
	c:dim7^5-
	cis:m5-
}

\score{
	<
		\context ChordNames \chord
		\context Staff \chord
	>
}

