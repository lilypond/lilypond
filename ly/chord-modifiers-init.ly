\version "1.7.3"

% urg!
%
\chordmodifiers #`(
	(m . ,(ly:make-pitch 0 2 -1 ))
	(min . ,(ly:make-pitch 0 2 -1 ))
	(aug . ,(ly:make-pitch 0 4 1 ))
	;; (dim . ,(ly:make-pitch -100 4 -1 ))	
	(dim . ,(ly:make-pitch -100 2 -1 ))
	;; urg, not actually a chord-modifier, but it works
	;;  c7 -> <c bes>, c 7+ -> c b
	(maj . ,(ly:make-pitch 0 6 1 ))
	;; sus4 should delete 2 too...
	(sus . ,(ly:make-pitch 0 3 0 ))
)

