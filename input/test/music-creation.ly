\version "2.8.0"

% possible rename to scheme- something.  -gp
\header { texidoc = "@cindex Scheme Music Creation
You can engrave music using just Scheme expressions. Although those 
expressions reflect the inner mechanism of LilyPond, they are rather 
clumsy to use, so avoid them, if possible. "
}

#(define (make-note-req p d)
   (make-music 'NoteEvent
    'duration d
    'pitch p))

#(define (make-note elts)
   (make-music 'EventChord
    'elements elts))

#(define (seq-music-list elts)
   (make-music 'SequentialMusic
    'elements elts))

fooMusic = #(seq-music-list
             (list (make-note (list (make-note-req (ly:make-pitch 1 0 0) (ly:make-duration 2 0))))
                   (make-note (list (make-note-req (ly:make-pitch 1 1 0) (ly:make-duration 2 0))))))
     
\score { \fooMusic 
\layout { ragged-right = ##t }
}

