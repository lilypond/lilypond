
;; todo: move this to engraver-init.ly 

(define-public context-description-alist
  '(
    (LyricsVoice . )
    (Thread . )
    (Voice . )


    (ChordNames . "
  Can contain @code{ChordNamesVoice}
    contexts.")

    (Lyrics . "
")
    (Staff .

    (RhythmicStaff .
    (GrandStaff . 
    (PianoStaff .
		

    (StaffGroup .
		
    (ChoirStaff . "
")
    (Score .
	   
")

    (TabStaff . 
    )
  )

(set! context-description-alist
      (sort context-description-alist alist<?))
