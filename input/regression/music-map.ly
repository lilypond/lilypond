\header {

texidoc =

"With @code{music-map}, you can apply functions operating on a single
piece of music to an entire music expression. In this example, the
scripts and dynamics of the first measure of music are applied to the
2nd measure. "

}


\version "1.7.8"

#(define (notes-to-skip m)
"Convert all stuff with duration (notes, lyrics, bass figures, etc.) to skips.
Scripts and dynamics are maintained.
"
  (if (memq 'rhythmic-event (ly:get-mus-property m 'types))
	(let* ((newmus 	  (make-music-by-name 'SkipEvent)))
		(map
		  (lambda (x) (ly:set-mus-property! newmus (car x) (cdr x)))
		  (ly:get-mutable-properties m))
		newmus
	)
	m)
)


foobar = \notes \transpose c c' { c4-\>-^ c4-^ c4-\!-^ c4-^  } 

\score {
  \notes \relative c''  \context Voice {
	\foobar

	< \apply #(lambda (x) (music-map x notes-to-skip))
		\foobar
	   { d2 d2 } > 
}}
