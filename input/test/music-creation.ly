\version "1.7.0"  %% or actually: 1.7.1 ...
\header {

 texidoc = "You can create music expressions from Scheme.  The
 mechanism for this is rather clumsy to use, so avoid if possible."

 }

#(define (make-note-req p d)
   (let* ((ml (make-music-by-name 'NoteEvent)))
   (ly-set-mus-property! ml 'duration d)
   (ly-set-mus-property! ml 'pitch p)
   ml))

#(define (make-note elts)
   (let* ((ml (make-music-by-name 'EventChord)))
   (ly-set-mus-property! ml 'elements elts)
   ml))

#(define (seq-music-list elts)
   (let* ((ml (make-music-by-name 'SequentialMusic)))
   (ly-set-mus-property! ml 'elements elts)
   ml))


fooMusic  = #(seq-music-list
  (list (make-note (list (make-note-req (make-pitch 1 0 0) (make-duration 2 0))))
     (make-note (list (make-note-req (make-pitch 1 1 0) (make-duration 2 0)))))
     )
     
\score { \fooMusic }
