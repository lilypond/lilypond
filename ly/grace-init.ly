

 #(define (grace-beam-space-function multiplicity)
         (* (if (<= multiplicity 3) 0.816 0.844) 0.8))


 #(define (make-text-checker text)
  (lambda (elt) (equal? text (ly-get-grob-property elt 'text))))


startGraceMusic = {

%{
from GraceContext
	Stem \override  #'flag-style = #"grace"
	Stem \override  #'stem-length = #6.0
	Stem \override  #'direction = #1

	NoteHead \override #'font-relative-size = #-1
	Stem \override #'font-relative-size = #-1
	Stem \override #'stem-shorten = #'(0)
	Beam \override #'font-relative-size = #-1
	TextScript \override #'font-relative-size = #-1
	Slur \override #'font-relative-size = #-1
	Accidentals \override #'font-relative-size = #-1
	Beam \override #'thickness = #0.3
	Beam \override #'space-function = #(lambda (x) 0.5)

	Stem \override #'lengths = #(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0))
	Stem \override #'beamed-lengths =
		#'(0.0 2.5 2.0 1.5)
	Stem \override #'beamed-minimum-lengths
		 = #(map (lambda (x) (* 0.8 x)) '(0.0 2.5 2.0 1.5))
%}


    \property Voice.NoteHead \override #'font-relative-size = #-1
    \property Voice.Stem \override #'length = #6
    \property Voice.Stem \override #'beamed-lengths =
        #(map (lambda (x) (* 1.25 x)) '(0.0 2.5 2.0 1.5))
    \property Voice.Stem \override #'beamed-minimum-lengths =
        #(map (lambda (x) (* 1.25 x)) '(0.0 1.5 1.25 1.0))
    \property Voice.Beam \override #'space-function = #grace-beam-space-function
    \property Voice.fontSize = #-2
    \property Voice.Stem \override #'no-stem-extend = ##t

%    \property Voice.Stem \override #'flag-style  = #"grace"
}

stopGraceMusic = {
% \property Voice.Stem \revert #'flag-style
    \property Voice.Stem \override #'no-stem-extend = ##f 
    \property Voice.Stem \revert #'length
    \property Voice.Stem \revert #'beamed-lengths
    \property Voice.Stem \revert #'beamed-minimum-lengths
    \property Voice.Beam \revert #'space-function
    \property Voice.fontSize \unset
}
