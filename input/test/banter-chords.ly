\version "1.3.96";

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

#;(define chord::names-alist-banter '())
#(set! chord::names-alist-banter
      (append 
      '(
        (((0 . 0) (2 . -1) (4 . -1)) . (("m" ("5-" . (type . "super")))))
	; Co iso Cm5-7-
        (((0 . 0) (2 . -1) (4 . -1) (6 . -2)) . ("o" (type "super")))
	)))

% German note names:
% Urg, this will break again, in time
% Is this correct, anyway?

#(define (pitch->text pitch)
  (if (and (= (modulo (cadr pitch) 7) 6)
	      (= (caddr pitch) -1))
      (cons (make-string 1 (integer->char 66)) '())
      (cons
       (if (= (modulo (cadr pitch) 7) 6)
	   (make-string 1 (integer->char 72))
	   (make-string 1 (integer->char (+ (modulo (+ (cadr pitch) 2) 7) 65))))
       (if (= (caddr pitch) 0)
	   '()
	   (list (list (string-append "accidentals-" 
				      (number->string (caddr pitch)))
		       '(font . "feta")))))))

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

