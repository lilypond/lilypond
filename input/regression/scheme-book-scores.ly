\version "2.23.2"

\header {

  texidoc = "Scores can be generated with scheme, too, and inserted into the
current book(part). Generated and explicit scores can be mixed, the header
informations from top- and booklevel stack correctly."

}

#(use-modules (lily display-lily))

% Sample score, which adds a score (containing just one note) to the current
% book/bookpart/at toplevel using scheme rather than the parser.
% That score is supposed to use the global header information, too.
#(define add-one-note-score 
   (let ((pitch 0))
        (lambda (parser)
          (let* ((scmpitch (ly:make-pitch 0 pitch 0))
                 (music (make-music 'EventChord
                          'elements (list (make-music 'NoteEvent
                                            'duration (ly:make-duration 2 0 1/1)
                                            'pitch scmpitch))))
                 (score (scorify-music music))
                 (layout (ly:output-def-clone $defaultlayout))
                 (desc (markup #:large #:line ((format #f "Score with a ~a"
                                (note-name->lily-string scmpitch))))))
            (ly:score-add-output-def! score layout)
            (add-text desc)
            (add-score score))
            (set! pitch (modulo (1+ pitch) 7)))))

oneNoteScore =
#(define-void-function () ()
   (add-one-note-score (*parser*)))

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

