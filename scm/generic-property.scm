
(define generic-beam-properties
  (cons "Beam"
	(list
	 (list 'beamSlopeDamping number? 'damping)
	 (list 'autoKneeGap number? 'auto-knee-gap)
	 (list 'autoInterstaffKneeGap number? 'auto-interstaff-knee-gap)
	 (list 'beamDirAlgorithm symbol? 'beam-dir-algorithm)
	 (list 'beamHeight number? 'height-hs)
	 (list 'beamVerticalPosition number? 'y-position-hs)
	 )
	)
  )


(define generic-stem-properties
  (cons "Stem"
	(list
	 (list 'stemVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)	 
	 (list 'stemLength number? 'length)
	 (list 'flagStyle string? 'flag-style)
	 (list 'stemCentered boolean? 'stem-centered)
	 (list 'noStemExtend boolean? 'no-stem-extend)
	 (list 'stemShorten number? 'shorten)
	 ))
  )

(define generic-dot-properties
  (cons "Dots" (list
		(list 'dotDirection dir? 'direction)
		(list 'verticalDirection dir? 'direction)
		)
	))

(define generic-script-properties
  (cons "Script" (list
		  (list 'articulationScriptVerticalDirection dir? 'direction)
		  (list 'articulationScriptPadding number? 'padding)

		  ))
  )



(define generic-text-properties
  (cons "Text_item" (list
		     (list 'textStyle string? 'style)
		     (list 'textScriptPadding number? 'padding)
		     (list 'textVerticalAlignment dir? 'self-alignment-Y)
		     (list 'textHorizontalAlignment dir? 'self-alignment-X)
		     )
	))

(define generic-crescendo-properties
  (cons "Crescendo" (list
		     (list 'verticalDirection dir? 'direction)
		     (list 'dynamicDirection dir? 'direction)
		     (list 'dynamicPadding number? 'padding) 
		     (list 'dynamicMinimumSpace number? 'minimum-space) 
		     )))
  
(define generic-dynamic-line-spanner-properties
  (cons "Dynamic_line_spanner" (list
		     (list 'verticalDirection dir? 'direction)
		     (list 'dynamicDirection dir? 'direction)
		     (list 'dynamicPadding number? 'padding) 
		     (list 'dynamicMinimumSpace number? 'minimum-space) 
		     )))
  
(define generic-volta-spanner-properties
  (cons "Volta_spanner" (list
		     (list 'voltaVerticalDirection dir? 'direction)
		     (list 'voltaPadding number? 'padding) 
		     (list 'voltaMinimumSpace number? 'minimum-space) 
		     )))
  
(define generic-bar-properties
  (cons "Staff_bar" (list
		     (list 'barSize number? 'bar-size))
	)
  )	

; don't do this yet. Depends on whennn the staff is really announced
(define generic-staff-symbol-properties
  (cons "Staff_symbol" (list
			)
	)
  )

(define generic-breathing-sign-properties
  (cons "Breathing_sign"
	(list
	 (list 'breathingSignVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)
	 )))

(define generic-clef-properties
  (cons "Clef_item"
	(list
	 (list 'clefStyle string? 'style))
	)
  )

(define generic-All-properties
  (cons "all"  (list (list 'fontSize number? 'fontsize))))

(define generic-rest-properties
  (cons "Rest" (list (list 'restStyle string? 'reststyle))))

(define generic-rest-collision-properties
  (cons "Rest_collision" (list (list 'maximumRestCount number? 'maximum-rest-count))))

(define generic-tie-properties
  (cons "Tie" (list
	       (list 'tieVerticalDirection dir? 'direction)
	       (list 'verticalDirection dir? 'direction)
  )))
(define generic-tie-column-properties
  (cons "Tie_column" (list
		      (list 'tieVerticalDirection dir? 'direction)
		      (list 'verticalDirection dir? 'direction)
  )))


(define generic-note-column-properties
  (cons "Note_column"
	(list
	 (list 'horizontalNoteShift number? 'horizontal-shift)
	 (list 'forceHorizontalShift number? 'force-hshift)
	 )))

(define generic-collision-properties
  (cons "Collision"
	(list
	 (list 'collisionMergeDotted boolean? 'merge-differently-dotted)
	 )
	)
  )
  
(define generic-slur-properties
  (cons "Slur"
	(list
	 (list 'slurVerticalDirection dir? 'direction)
	 (list 'verticalDirection dir? 'direction)	 
	 (list 'slurDash number? 'dashed))))

(define generic-timesig-properties
  (cons "Time_signature"
	(list
	 (list 'timeSignatureStyle string? 'style))))

(define (symbol-or-boolean? s)
  (or (boolean? s) (symbol? s)))

(define generic-tuplet-spanner-properties
  (cons "Tuplet_spanner"
	(list
	 (list 'tupletNumberVisibility symbol-or-boolean? 'tuplet-number-visibility)
	 (list 'tupletBracketVisibility symbol-or-boolean? 'tuplet-bracket-visibility)
	))
)

(define generic-voice-properties
  (list
   generic-stem-properties
   generic-breathing-sign-properties
   generic-crescendo-properties
   generic-dynamic-line-spanner-properties
   generic-tie-properties
   generic-tie-column-properties   
   generic-tuplet-spanner-properties
   generic-rest-properties
   generic-slur-properties
   generic-beam-properties
   generic-text-properties
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
	generic-dot-properties
	)

  )
   
(define generic-lyrics-properties
  (list generic-text-properties
  )
  
)
