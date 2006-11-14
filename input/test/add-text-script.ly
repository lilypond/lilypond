\version "2.10.0"
\sourcefilename "add-text-script.ly"

\header {
texidoc= "@cindex make-music Fingering
You can add various stuff to notes using @code{make-music}.
In this example, an extra fingering is attached to a note. 
"
} 

#(define (make-text-script x) 
   (make-music 'TextScriptEvent
               'direction DOWN
               'text (make-simple-markup x)))

#(define (add-text-script m x)
   (if (equal? (ly:music-property m 'name) 'EventChord)
       (set! (ly:music-property m 'elements)
             (cons (make-text-script x)
                  (ly:music-property m 'elements)))       
       (let ((es (ly:music-property m 'elements))
	     (e (ly:music-property m 'element)))
	 (map (lambda (y) (add-text-script y x)) es)
	 (if (ly:music? e)
	     (add-text-script e x))))
   m)

addScript =
#(define-music-function (parser location script music )
					( string? ly:music? )
		(add-text-script music script))

\score {
  {
    \addScript "6" { c'4-3 }
  }
}

