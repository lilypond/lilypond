
; BROKEN as of 1.3.55, FIXME
;


;
; Format: 
; (cons 'Type name
;    (list PROPERTYDESCRIPTIONS))
; 
; where
; PROPERTYDESCRIPTION
; is
;
;  (list 'translatorPropertySymbol type-predicate backend-property-symbol)
;
; Descriptions earlier in the list take precedence over later descriptions.
;


(define generic-beam-properties
  (cons 'beam-interface
	(list
	 (list 'beamSlopeDamping number? 'damping)
	 (list 'autoKneeGap number? 'auto-knee-gap)
	 (list 'autoInterstaffKneeGap number? 'auto-interstaff-knee-gap)
	 (list 'beamDirAlgorithm symbol? 'beam-dir-algorithm)
	 (list 'beamHeight number? 'height)
	 (list 'beamVerticalPosition number? 'staff-position)
	 )
	)
  )


(define generic-stem-properties
  (cons 'stem-interface
	(list
	 (list 'stemVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)	 
	 (list 'stemLength number? 'length)
	 (list 'flagStyle string? 'flag-style)
	 (list 'noStemExtend boolean? 'no-stem-extend)
	 (list 'stemShorten number? 'shorten)
	 ))
  )

(define generic-dot-properties
  (cons 'dot-interface
	(list
		(list 'dotDirection dir? 'direction)
		(list 'verticalDirection dir? 'direction)
		)
	))

(define generic-script-properties
  (cons 'script-interface
	(list
		  (list 'articulationScriptVerticalDirection dir? 'direction)
		  (list 'articulationScriptPadding number? 'padding)

		  ))
  )



(define generic-text-properties
  (cons 'text-item-interface
	(list
		     (list 'textStyle string? 'style)
		     (list 'lyricAlignment dir? 'self-alignment-X)
		     (list 'textScriptPadding number? 'padding)
		     (list 'textVerticalAlignment dir? 'self-alignment-Y)
		     (list 'textHorizontalAlignment dir? 'self-alignment-X)
		     (list 'textScriptWordSpace number? 'word-space)
		     )
	))

(define generic-sustain-pedal-properties
  (cons 'sustain-pedal-interface
	(list
	 (list 'sustainPedalPadding number? 'padding))))

(define generic-chord-name-properties
  (cons 'chord-name-interface
	(list
	 (list 'textScriptWordSpace number? 'word-space)
	 (list 'chordNameWordSpace number? 'word-space)
	 (list 'chordNameStyle string? 'style))))

(define generic-crescendo-properties
  (cons 'crescendo-interface
	(list
	 (list 'dynamicDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)
	 (list 'dynamicPadding number? 'padding) 
	 (list 'dynamicMinimumSpace number? 'minimum-space) 
	 )))

(define generic-dynamic-line-spanner-properties
  (cons 'dynamic-interface
	(list
		     (list 'dynamicDirection dir? 'direction)
		     (list 'verticalDirection dir? 'direction)
		     (list 'dynamicPadding number? 'padding) 
		     (list 'dynamicMinimumSpace number? 'minimum-space) 
		     )))
  
(define generic-volta-spanner-properties
  (cons 'volta-spanner-interface (list
		     (list 'voltaVerticalDirection dir? 'direction)
		     (list 'voltaPadding number? 'padding) 
		     (list 'voltaMinimumSpace number? 'minimum-space) 
		     )))
  
(define generic-bar-properties
  (cons 'staff-bar-interface
	(list
	 (list 'barSize number? 'bar-size))
	)
  )	

(define generic-bar-number-properties
  (cons 'bar-number-interface
	(list
	 (list 'barNumberScriptPadding number? 'padding)
	 (list 'barNumberDirection dir? 'direction)	 
	 )
	
	)
  )

  
; don't do this yet. Depends on whennn the staff is really announced
(define generic-staff-symbol-properties
  (cons 'staff-symbol-interface
	(list
	 )
	)
  )

(define generic-breathing-sign-properties
  (cons 'breathing-sign-interface
	(list
	 (list 'breathingSignVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)
	 )))

(define generic-clef-properties
  (cons 'clef-interface
	(list
	 (list 'clefStyle string? 'style))
	)
  )

(define generic-All-properties
  (cons 'all
	(list (list 'fontSize number? 'font-relative-size))))


(define generic-notehead-properties
  (cons 'note-head-interface
	(list (list 'noteHeadStyle symbol? 'style))))

(define generic-notename-properties
  (cons 'note-name-interface
	(list (list 'noteNameStyle symbol? 'style))))


(define generic-rest-properties
  (cons 'rest-interface
	(list (list 'restStyle string? 'reststyle))))

(define generic-rest-collision-properties
  (cons 'rest-collision-interface
	(list (list 'maximumRestCount number? 'maximum-rest-count))))

(define generic-tie-properties
  (cons 'tie-interface
	(list
	       (list 'tieVerticalDirection dir? 'direction)
	       (list 'verticalDirection dir? 'direction)
(list 'tieDash number? 'dashed)
  )))

(define generic-tie-column-properties
  (cons 'tie-column-interface (list
		      (list 'tieVerticalDirection dir? 'direction)
		      (list 'verticalDirection dir? 'direction)
  )))

(define generic-note-column-properties
  (cons 'note-column-interface
	(list
	 (list 'horizontalNoteShift number? 'horizontal-shift)
	 (list 'forceHorizontalShift number? 'force-hshift)
	 )))

(define generic-collision-properties
  (cons 'collision-interface
	(list
	 (list 'collisionMergeDotted boolean? 'merge-differently-dotted)
	 )
	)
  )
  
(define generic-slur-properties
  (cons 'slur-interface
	(list
	 (list 'slurVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)	 
	 (list 'slurDash number? 'dashed))))

(define generic-timesig-properties
  (cons 'time-signature-interface
	(list
	 (list 'timeSignatureStyle string? 'style))))

(define (symbol-or-boolean? s)
  (or (boolean? s) (symbol? s)))

(define generic-tuplet-spanner-properties
  (cons 'tuplet-spanner-interface
	(list
	 (list 'tupletDirection dir? 'direction)
	 (list 'tupletNumberVisibility symbol-or-boolean? 'tuplet-number-visibility)
	 (list 'tupletBracketVisibility symbol-or-boolean? 'tuplet-bracket-visibility)
	))
)





;;;;;;;;;;
;; don't forget to add these to the Generic_property_list property in
;; engraver.ly

(define generic-voice-properties
  (list
   generic-stem-properties
   generic-breathing-sign-properties
   generic-crescendo-properties
   generic-dynamic-line-spanner-properties
   generic-tie-properties
   generic-tie-column-properties   
   generic-tuplet-spanner-properties
   generic-notehead-properties
   generic-rest-properties
   generic-slur-properties
   generic-beam-properties
   generic-text-properties
   generic-sustain-pedal-properties
   generic-note-column-properties
   generic-script-properties
   generic-All-properties
   ))

(define generic-grace-properties generic-voice-properties)
(define generic-staff-properties
  (list
   generic-text-properties   
   generic-bar-properties
   generic-timesig-properties
   generic-clef-properties
   generic-collision-properties
   generic-sustain-pedal-properties
   generic-rest-collision-properties
   generic-volta-spanner-properties
;   generic-staff-symbol-properties
   generic-All-properties      
   )
  )
(define generic-grand-staff-properties
  (list
   generic-text-properties   
   generic-bar-properties
   ))

(define generic-thread-properties
  (list generic-All-properties
        generic-notehead-properties
	generic-dot-properties
	)

  )
   
(define generic-lyrics-properties
  (list generic-text-properties
  )
)

(define generic-chord-staff-properties
  (list generic-chord-name-properties
  )
)
