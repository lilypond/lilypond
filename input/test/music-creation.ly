\version "2.1.26"

% possible rename to scheme- something.  -gp
\header { texidoc = "@cindex Scheme Music Creation
You can create music expressions from Scheme.  The
mechanism for this is rather clumsy to use, so avoid it if possible. "
}

#(define (make-note-req p d)
   (let* ((ml (make-music-by-name 'NoteEvent)))
   (ly:music-set-property! ml 'duration d)
   (ly:music-set-property! ml 'pitch p)
   ml))

#(define (make-note elts)
   (let* ((ml (make-music-by-name 'EventChord)))
   (ly:music-set-property! ml 'elements elts)
   ml))

#(define (seq-music-list elts)
   (let* ((ml (make-music-by-name 'SequentialMusic)))
   (ly:music-set-property! ml 'elements elts)
   ml))


fooMusic = #(seq-music-list
  (list (make-note (list (make-note-req (ly:make-pitch 1 0 0) (ly:make-duration 2 0))))
     (make-note (list (make-note-req (ly:make-pitch 1 1 0) (ly:make-duration 2 0)))))
     )
     
\score { \fooMusic 
\paper { raggedright = ##t }
}

