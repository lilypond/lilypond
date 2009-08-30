\version "2.13.4"

\header {

  texidoc = "Scores can be generated with scheme, too, and inserted into the
current book(part). Generated and explicit scores can be mixed, the header
informations from top- and booklevel stack correctly."

}

#(use-modules (scm display-lily))

% Sample score, which adds a score (containing just one note) to the current
% book/bookpart/at toplevel using scheme rather than the parser.
% That score is supposed to use the global header information, too.
#(define add-one-note-score 
   (let ((pitch 0))
        (lambda (parser)
          (let* ((scmpitch (ly:make-pitch 0 pitch 0))
                 (music (make-music 'EventChord
                          'elements (list (make-music 'NoteEvent
                                            'duration (ly:make-duration 2 0 1 1)
                                            'pitch scmpitch))))
                 (score (scorify-music music parser))
                 (layout (ly:output-def-clone $defaultlayout))
                 (desc (markup #:large #:line ((ly:format "Score with a ~a"
                                (note-name->lily-string scmpitch parser))))))
            (ly:score-add-output-def! score layout)
            (add-text parser desc)
            (add-score parser score))
            (set! pitch (modulo (1+ pitch) 7)))))

oneNoteScore =
#(define-music-function (parser location) ()
   (add-one-note-score parser)
   (make-music 'Music 'void #t))

%%%

\header {
  title = "Main Title"
  subtitle = "Main subtitle"
  piece = "Piecetitle"
}

\oneNoteScore

\bookpart {
  \header { title ="Title 1" subtitle="Sub1"}
  \oneNoteScore
  \score { \relative c' c1 }
  \oneNoteScore
}


\bookpart {
  \score { \relative c' c1 }
  \oneNoteScore
}

\oneNoteScore

