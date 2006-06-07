\version "2.9.7"
\sourcefilename "add-staccato.ly"

\header {
texidoc= "@cindex Add Stacato
Using @code{make-music}, you can add various stuff to notes. In this
example staccato dots are added to the notes."
} 

#(define (make-script x)
   (make-music 'ArticulationEvent
               'articulation-type x))

#(define (add-script m x)
   (if
     (equal? (ly:music-property m 'name) 'EventChord)
     (set! (ly:music-property m 'elements)
           (cons (make-script x)
                 (ly:music-property m 'elements))))
   m)

#(define (add-staccato m)
   (add-script m "staccato"))

addStacc =
#(define-music-function (parser location music) 
					(ly:music?)
		(music-map add-staccato music))	   

\score {
  \relative c'' {
    a b \addStacc { c c } 
  }
  \layout{ ragged-right = ##t }
}

