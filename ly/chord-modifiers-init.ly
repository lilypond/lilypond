\version "1.5.68"

% urg!
%
\chordmodifiers #`(
	(m . ,(make-pitch 0 2 -1 ))
	(min . ,(make-pitch 0 2 -1 ))
	(aug . ,(make-pitch 0 4 1 ))
	;; (dim . ,(make-pitch -100 4 -1 ))	
	(dim . ,(make-pitch -100 2 -1 ))
	;; urg, not actually a chord-modifier, but it works
	;;  c7 -> <c bes>, c 7+ -> c b
	(maj . ,(make-pitch 0 6 1 ))
	;; sus4 should delete 2 too...
	(sus . ,(make-pitch 0 3 0 ))
)

