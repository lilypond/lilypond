
(define generic-beam-properties
  (cons "Beam"
	(list
	 (list 'beamslopedamping number? 'damping)
	 (list 'autoKneeGap number? 'auto-knee-gap)
	 (list 'autoInterstaffKneeGap number? 'auto-interstaff-knee-gap)
	 (list 'beamQuantisation symbol? 'slope-quantisation)
	 (list 'beamDirAlgorithm symbol? 'beam-dir-algorithm)
	 )
	)
  )


(define generic-stem-properties
  (cons "Stem"
	(list
	 (list 'stemLength number? 'length)
	 (list 'stemStyle string? 'style)
	 (list 'noStemExtend boolean? 'no-stem-extend)
	 ))
  )

(define generic-text-properties
  (cons "Text_item" (list
		     (list 'textStyle string? 'style)
		     (list 'textScriptPadding number? 'padding)
		     )
	))

(define generic-bar-properties
  (cons "Staff_bar" (list
		     (list 'barSize number? 'bar-size))
	)
  )	
(define generic-breathing-sign-properties
  (cons "Breathing_sign"
	(list
	 (list 'breathingSignBreakPriority number? 'break-priority
	  ))))

(define generic-clef-properties
  (cons "Clef_item"
	(list
	 (list 'clefBreakPriority number? 'break-priority)
	 (list 'clefStyle string? 'style))
	)
  )

(define generic-All-properties
  (cons "all"  (list (list 'fontSize number? 'fontsize))))

(define generic-rest-properties
  (cons "Rest" (list (list 'restStyle string? 'reststyle))))

(define generic-note-column-properties
  (cons "Note_column"
	(list
	 (list 'horizontalNoteShift number? 'horizontal-shift)
	 (list 'forceHorizontalShift number? 'force-hshift)
	 )))

(define generic-slur-properties
  (cons "Slur"
	(list
	 (list 'slurDash number? 'dashed))))

(define generic-timesig-properties
  (cons "Time_signature"
	(list
	 (list 'timeSignatureStyle string? 'sigstyle))))

(define generic-voice-properties
  (list
   generic-stem-properties
   generic-rest-properties
   generic-slur-properties
   generic-beam-properties
   generic-text-properties
   generic-note-column-properties
   generic-All-properties
   ))

(define generic-grace-properties generic-voice-properties)
(define generic-staff-properties
  (list
   generic-bar-properties
   generic-timesig-properties
   generic-clef-properties
   generic-All-properties      
   )
  )

(define generic-thread-properties
  (list generic-All-properties))
   
