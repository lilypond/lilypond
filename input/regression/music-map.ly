\header {

texidoc =

"With @code{music-map}, you can apply functions operating on a single
piece of music to an entire music expression. In this example, the the
function @code{notes-to-skip} changes a note to a skip. When applied
to an entire music expression in the 1st measure, the scripts and
dynamics are left over. These are put onto the 2nd measure."

}

\version "1.7.18"

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

	< \apply #(lambda (x) (music-map notes-to-skip x))
		\foobar
	   { d2 d2 } > 
}}
