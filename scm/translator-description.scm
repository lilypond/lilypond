
(define (engraver-description name description created-elts properties)
  (list name description created-elts properties)
  )

(define (translator-property-description symbol type? description)
  (list symbol type? description)
  )

(define engraver-description-alist
  (list
   (cons
   'Stem_engraver
   (engraver-description
    "Stem_engraver"
    "Create stems and single-stem tremolos"
    '(Stem StemTremolo)
    (list
     (translator-property-description 'tremoloFlags integer? "")
     (translator-property-description 'stemLeftBeamCount integer? "")
     (translator-property-description 'stemRightBeamCount integer? "")    
     )))
   
  (cons
   'Hyphen_engraver
   (engraver-description
    "Hyphen_engraver"
    "Create lyric hyphens"
    '(LyricHyphen)
    (list
     )))

  (cons
   'Extender_engraver
   (engraver-description
    "Extender_engraver"
    "Create lyric extenders"
    '(LyricExtender)
    (list
     )))

  
  (cons
   'Separating_line_group_engraver
   (engraver-description
    "Separating_line_group_engraver"
    "Objects that generate rods and springs for the spacing problem."
    '(SeparationItem SeparatingGroupSpanner)
    (list
     )))

  (cons
   'Axis_group_engraver
   (engraver-description
    "Axis_group_engraver"
    "Group all objects created in this context in a VerticalAxisGroup spanner."
    '(VerticalAxisGroup)
    (list
     (translator-property-description 'CONTEXTNAMEVerticalExtent number-pair? "hard coded vertical extent [fixme, naming]")
     (translator-property-description 'CONTEXTNAMEMinimumVerticalExtent number-pair? "minimum vertical extent [fixme, naming]")
     (translator-property-description 'CONTEXTNAExtraVerticalExtent number-pair? "extra vertical extent [fixme, naming]")          
     )))

  (cons
   'Hara_kiri_engraver
   (engraver-description
    "Hara_kiri_engraver"
    "Like Axis_group_engraver, but make a hara kiri spanner, and add
interesting items (ie. note heads, lyric syllables and normal rests)"
    '(HaraKiriVerticalGroup)
    '()
    ))

  
  (cons
   'Local_key_engraver
   (engraver-description
    "Local_key_engraver"
    "Make accidentals.  Catches note heads, ties and notices key-change
   events.  Due to interaction with ties (which don't come together
   with note heads), this needs to be in a context higher than Tie_engraver.
   (FIXME)."
    '(Accidentals)
    (list
     (translator-property-description 'localKeySignature list? "the key signature at this point  in the measure")
     (translator-property-description 'forgetAccidentals boolean? "do
not set localKeySignature when a note alterated differently from
localKeySignature is found.
<p>
Causes accidentals to be printed at every note instead of
remembered for the duration of a measure.
")
     (translator-property-description 'noResetKey boolean? "Do not
reset local key to the value of keySignature at the start of a measure,
as determined by measurePosition.<p>
    Do not reset the key at the start of a measure.  Accidentals will
    be printed only once and are in effect until overridden, possibly
    many measures later.
")
       
   )))

  
  (cons
   'Volta_engraver
   (engraver-description
    "Volta_engraver"
    "Make volta brackets"
    '(VoltaBracket)
    (list
     (translator-property-description 'repeatCommands list?
"This property is read to find any command of the form (volta . X), where X is a string or #f")
     (translator-property-description 'voltaSpannerDuration moment?
"maximum duration of the volta bracket.<p>

    Set to a duration to control the size of the brackets printed by
@code{\alternative}.  It specifies the number of whole notes duration
to use for the brackets.  This can be used to shrink the length of
brackets in the situation where one alternative is very large.  It may
have odd effects if the specified duration is longer than the music
given in an @code{\alternative}.
")
       )
   ))

  (cons
   'Clef_engraver
   (engraver-description
    "Clef_engraver"
    "Determine and set reference point for pitches"
    '(Clef OctavateEight)
    (list
     (translator-property-description 'supportedClefTypes
				      list? "Clef settings supported. The value is an association list contain entries (NAME . (GLYPH . POSITION)), where  NAME is the clef name (alto, baritone, etc.), GLYPH the glyph name, POSITION an integer where the center symbol should go.")
     (translator-property-description 'clefPosition number? " where the center of the symbol should go")
     (translator-property-description 'clefGlyph string? "name of the symbol within the music font")
     (translator-property-description 'centralCPosition number? "place of the central C. ")
     (translator-property-description 'defaultClef string? "generate this clef at the very start of this staff. If not set, don't generate a clef")
     (translator-property-description 'explicitClefVisibility procedure? "visibility-lambda function for clefs entered as \clef.")
     (translator-property-description 'clefPitches list? "alist mapping GLYPHNAME to the position of the central C for that symbol")

     )))


  ))
