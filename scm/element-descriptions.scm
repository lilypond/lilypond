
; distances are given in stafflinethickness (thicknesses) and
; staffspace (distances)

(define all-element-descriptions
  `((Arpeggio . (
	       (X-extent-callback . ,Arpeggio::width_callback)
	       (molecule-callback . ,Arpeggio::brew_molecule)
	       (Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	       (X-offset-callbacks . (,Side_position::aligned_side))
	       (direction . -1)
	       (staff-position . 0.0)
	       (meta . ,(element-description "Arpeggio" arpeggio-interface side-position-interface))
	       ))
  
	(BarLine . (
		(break-align-symbol . Staff_bar)
		(glyph . "|")
		(break-glyph-function . ,default-break-barline)
		(barsize-procedure . ,Bar::get_staff_bar_size)
		(molecule-callback . ,Bar::brew_molecule)	   
		(visibility-lambda . ,all-visible)
		(breakable . #t)
		(before-line-breaking-callback . ,Bar::before_line_breaking)
		;;
		;; Ross. page 151 lists other values, we opt for a leaner look
		;; 
		(kern . 3.0)
		(thin-kern . 3.0)
		(hair-thickness . 1.6)
		(thick-thickness . 6.0)
		(meta . ,(element-description  "BarLine" bar-line-interface ))
	))

	(BarNumber . (
		(molecule-callback . ,Text_item::brew_molecule)
		(breakable . #t)
		(visibility-lambda . ,begin-of-line-visible)
		(padding . 1.0)
		(direction . 1)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
                (font-family . "roman")
		(meta . ,(element-description "BarNumber"
			text-interface  break-aligned-interface))
	))

	(Beam . ,basic-beam-properties)
	 
	(BreakAlignment . (
		(breakable . #t)
		(stacking-dir . 1)
		(axes 0)
		(X-offset-callbacks . (,Break_align_interface::self_align_callback))
		(space-alist . ,default-break-align-space-alist) 
		(meta . ,(element-description "BreakAlignment"
			axis-group-interface align-interface
			)
		)
	))

	(BreakAlignGroup . (
		(axes  . (0))
		(X-offset-callbacks . (,Break_align_interface::alignment_callback))
		
		(meta . ,(element-description "BreakAlignGroup" axis-group-interface))
	))

	(BreathingSign . (
		(break-align-symbol . Breathing_sign)
		(breakable . #t )
		(molecule-callback . ,Breathing_sign::brew_molecule)
		(Y-offset-callbacks . (,Breathing_sign::offset_callback))
		(visibility-lambda . ,begin-of-line-invisible)
		(meta . ,(element-description "BreathingSign"  break-aligned-interface))
	))

	(Clef . (
   	   (molecule-callback . ,Clef::brew_molecule)
	   (before-line-breaking-callback . ,Clef::before_line_breaking)
	   (breakable . #t)
	   (break-align-symbol . Clef_item)
	   (visibility-lambda . ,begin-of-line-visible)
	   (Y-offset-callbacks  . (,Staff_symbol_referencer::callback)) 
	   (meta . ,(element-description "Clef" clef-interface break-aligned-interface ))
	))

	(ChordNames . (
		(molecule-callback . ,Chord_name::brew_molecule)
		(after-line-breaking-callback . ,Chord_name::after_line_breaking)
		(chord-name-function . ,default-chord-name-function)
		(properties-to-font-name . ,properties-to-font-name)
		(style-to-font-name . ,style-to-font-name)
		(markup-to-properties . ,markup-to-properties)
		(font-size . "0") ;; Hmm, 0 should be the default, maybe??
		(font-family . "roman")
		(meta . ,(element-description "ChordNames" chord-name-interface))
	))

	(NoteCollision . (
		(axes 0 1)
		(note-width . 1.65)
		(meta . ,(element-description "NoteCollision"
		   note-collision-interface axis-group-interface
		))
	))

	(Crescendo . (
		(molecule-callback . ,Crescendo::brew_molecule)
		(thickness . 1.0)
		(shorten-for-letter  .  4.0)
		(height . 0.6666)
		(dash-thickness . 1.2)
		(dash-length . 4.0)
		(self-alignment-Y . 0)
		(Y-offset-callbacks . (,Side_position::aligned_on_self))
		(meta . ,(element-description "Crescendo" hairpin-interface))
	))

	(DotColumn . (
		(axes 0 )
		(meta . ,(element-description "DotColumn" dot-column-interface  axis-group-interface))
	))

	(Dots . (
		(molecule-callback . ,Dots::brew_molecule)
		(dot-count . 1)
		(staff-position . 0.0)
		(Y-offset-callbacks  . (,Dots::quantised_position_callback ,Staff_symbol_referencer::callback))
		(meta . ,(element-description "Dots" dot-interface ))
	))
	
	(DynamicText . (
		(Y-offset-callbacks . (,Side_position::aligned_on_self))
		(molecule-callback . ,Text_item::brew_molecule)
		(script-priority . 100)
		(font-style . dynamic)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		(self-alignment-Y . 0)

		(meta . ,(element-description "DynamicText" text-interface ))
	))
	
	(DynamicLineSpanner . (
		(axes . ( 1))
		(padding . 3)
		(minimum-space . 6)
		(meta . ,(element-description "DynamicLineSpanner" dynamic-interface axis-group-interface side-position-interface))
	))
	
	(LeftEdge . (
		(break-align-symbol . Left_edge_item)
		(breakable . #t)
		(meta . ,(element-description "LeftEdge" break-aligned-interface))
	))
	
	(Fingering . (
		(molecule-callback . ,Text_item::brew_molecule)
		(padding . 	3.0)
		(self-alignment-X . 0)
		(font-style . dynamic)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		
		(meta . ,(element-description "Fingering" finger-interface text-script-interface text-interface side-position-interface))
	))

	(GraceAlignment . (
		(axes . (0))
		(horizontal-space . 1.2)
		(padding . 1.0)
		(before-line-breaking-callback . ,Grace_align_item::before_line_breaking)
		(meta . ,(element-description "GraceAlignment" axis-group-interface align-interface grace-alignment-interface))
	))
	
	(HaraKiriVerticalGroup . (
		(Y-offset-callbacks . (,Hara_kiri_group_spanner::force_hara_kiri_callback))
		(Y-extent-callback . ,Hara_kiri_group_spanner::y_extent)
		(axes 1)
		(meta . ,(element-description "HaraKiriVerticalGroup" axis-group-interface hara-kiri-group-interface))
	))

	(LyricHyphen . (
		(thickness . 1.0)
		(height . 0.4)
		(minimum-length .  0.5) 
		(molecule-callback . ,Hyphen_spanner::brew_molecule)
		(Y-extent-callback . ,Score_element::point_dimension_callback)
		(meta . ,(element-description "LyricHyphen" lyric-hyphen-interface ))
	))
	
	(InstrumentName . (
		(breakable . #t)
		(Y-offset-callbacks . (,Side_position::centered_on_parent
				       ,Side_position::aligned_on_self))
		(self-alignment-Y . 0)
		(molecule-callback . ,Text_item::brew_molecule)		
		(break-align-symbol . Instrument_name)
		(visibility-lambda . ,begin-of-line-visible)
 		(properties-to-font-name . ,properties-to-font-name)
 		(style-to-font-name . ,style-to-font-name)
 		(markup-to-properties . ,markup-to-properties)
 		(font-family . "roman")
		(meta . ,(element-description "InstrumentName"  text-interface break-aligned-interface))
	))
	
	(KeySignature . (
  	  (molecule-callback . ,Key_item::brew_molecule)
	  (break-align-symbol . Key_item)
	  (visibility-lambda . ,begin-of-line-visible)
	  (breakable . #t)
	  (meta . ,(element-description "KeySignature" key-signature-interface  break-aligned-interface))
	))
	
	(Accidentals . (
		(molecule-callback . ,Local_key_item::brew_molecule)
		(X-offset-callbacks . (,Side_position::aligned_side))
		(direction . -1)
		(left-padding . 0.2)
		(right-padding . 0.4)
		(meta . ,(element-description "Accidentals"  accidentals-interface))
	))
	
	(LineOfScore . (
		(axes . (0 1))
		(meta . ,(element-description "LineOfScore"  line-of-score-interface axis-group-interface))
	))
	
	(LyricExtender . (
		(molecule-callback . ,Lyric_extender::brew_molecule)
		(height . 0.8) ; stafflinethickness;
		(right-trim-amount . 0.5)
		(Y-extent-callback . ,Score_element::point_dimension_callback)
		(meta . ,(element-description "LyricExtender"  lyric-extender-interface))
	))
	
	(LyricText . (
		(molecule-callback . ,Text_item::brew_molecule)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(self-alignment-X . 0)
		(non-rhythmic . #t)
		(word-space . 0.6)
		
		(properties-to-font-name . ,properties-to-font-name)
		(style-to-font-name . ,style-to-font-name)
		(markup-to-properties . ,markup-to-properties)
		(font-family . "roman")
                
		(meta . ,(element-description "LyricText" lyric-syllable-interface text-interface))
	))
	
	(RehearsalMark . (
	  (molecule-callback . ,Text_item::brew_molecule)	
	  (breakable . #t)
	  (properties-to-font-name . ,properties-to-font-name)
          (style-to-font-name . ,style-to-font-name)
              (markup-to-properties . ,markup-to-properties)
               (font-style . mark)
		 (visibility-lambda . ,end-of-line-invisible)
	  (padding . 4.0)
	  (meta . ,(element-description "RehearsalMark"  mark-interface side-position-interface))
	))
	
	(MultiMeasureRest . (
		(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
		(molecule-callback . ,Multi_measure_rest::brew_molecule)
		(staff-position . 0)
		(expand-limit . 10)
		(padding . 2.0) ; staffspace
		(minimum-width . 12.5) ; staffspace
 		(properties-to-font-name . ,properties-to-font-name)
 		(style-to-font-name . ,style-to-font-name)
 		(markup-to-properties . ,markup-to-properties)
 		(font-family . "number")
		(meta . ,(element-description "MultiMeasureRest" multi-measure-rest-interface ))
	))
	
	(NoteColumn . (
		(axes . (0 1))
		(meta . ,(element-description "NoteColumn"  axis-group-interface note-column-interface))
	))

	(NoteHead . (
		(style . default)
		(molecule-callback . ,Note_head::brew_molecule)
		(Y-offset-callbacks  . (,Staff_symbol_referencer::callback)) 
		(meta . ,(element-description  "NoteHead"
			rhythmic-head-interface
			note-head-interface ))
	))

	(NoteName . (
		(style . default)
		(molecule-callback . ,Text_item::brew_molecule)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		(font-family . "roman")
		(font-size . "0")
		(meta . ,(element-description  "NoteName"
			note-name-interface
			general-element-interface))
	))

	(OctavateEight . (
		(self-alignment-X . 0)
		(text . "8")
		(visibility-lambda . ,begin-of-line-visible)
		(X-offset-callbacks . (,Side_position::centered_on_parent ,Side_position::aligned_on_self))
		(Y-offset-callbacks . (,Side_position::aligned_side))
		(molecule-callback . ,Text_item::brew_molecule)
                (font-shape . "italic")
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)

		(meta . ,(element-description "OctavateEight" text-interface ))
	))
	
	(PaperColumn . (
		(axes 0)
                (before-musical-spacing-factor . 0.4)
 		(meta . ,(element-description "PaperColumn" paper-column-interface axis-group-interface spaceable-element-interface))
	))
	(NonMusicalPaperColumn . (
                (axes 0)
                (before-musical-spacing-factor . 1.0)
 		(meta . ,(element-description "NonMusicalPaperColumn" paper-column-interface axis-group-interface spaceable-element-interface))
        ))
	
	(Rest . (
		(after-line-breaking-callback . ,Rest::after_line_breaking)
		(molecule-callback . ,Rest::brew_molecule)
		(minimum-beam-collision-distance . 1.5)
		(meta . ,(element-description  "Rest"
			rhythmic-head-interface
			rest-interface ))
	))
	(RestCollision . (
		(minimum-distance . 0.75)
		(meta . ,(element-description "RestCollision" rest-collision-interface ))
	))

	(Script . (
		(molecule-callback . ,Script::brew_molecule)
		(X-offset-callbacks . (,Side_position::centered_on_parent))
		(meta . ,(element-description "Script" script-interface side-position-interface))
	))
	
	(ScriptColumn . (
		(before-line-breaking-callback . ,Script_column::before_line_breaking)
		(meta . ,(element-description "ScriptColumn" script-column-interface))
	))
	
	(Slur . ,default-basic-slur-properties)
	(SpacingSpanner . (
		(spacing-procedure . ,Spacing_spanner::set_springs)

		;; assume that notes at least this long are present.
		(maximum-duration-for-spacing . ,(make-moment 1 8))
		(meta . ,(element-description "SpacingSpanner"  spacing-spanner-interface))
	))
	(SpanBar . (

		(break-align-symbol . Staff_bar)
		(barsize-procedure . ,Span_bar::get_bar_size) 
		(molecule-callback . ,Bar::brew_molecule)
		(visibility-lambda . ,begin-of-line-invisible)
		(X-extent-callback . ,Span_bar::width_callback)
		(Y-offset-callbacks . (,Span_bar::center_on_spanned_callback))
		
		(breakable . #t)
		(glyph . "|")
		(before-line-breaking-callback . ,Span_bar::before_line_breaking)
		;; ugh duplication! 
		
		;;
		;; Ross. page 151 lists other values, we opt for a leaner look
		;; 
		(kern . 3.0)
		(thin-kern . 3.0)
		(hair-thickness . 1.6)
		(thick-thickness . 6.0)
		(meta . ,(element-description "SpanBar" span-bar-interface bar-line-interface ))
	))

	(StanzaNumber . (
		(breakable . #t)
		(molecule-callback . ,Text_item::brew_molecule)		
		(break-align-symbol . Clef_item)
		(visibility-lambda . ,begin-of-line-visible)
		(properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		(font-family . "roman")
(meta . ,(element-description "StanzaNumber" break-aligned-interface text-interface))
	))

	(StaffSymbol . (
		(molecule-callback . ,Staff_symbol::brew_molecule)
		(staff-space . 1.0)
		(line-count . 5 )
		(meta . ,(element-description "StaffSymbol" staff-symbol-interface ))
	))
	(SostenutoPedal . (
		(molecule-callback . ,Text_item::brew_molecule)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))
		(no-spacing-rods . #t)
                (font-shape . "italic")
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		
		(self-alignment-X . 0)
		(meta . ,(element-description "SostenutoPedal" text-interface ))
	))

	(Stem . (
		(before-line-breaking-callback . ,Stem::before_line_breaking)
		(molecule-callback . ,Stem::brew_molecule)
		(thickness . 0.8)
		(beamed-lengths . (0.0 2.5 2.0 1.5))
		(beamed-minimum-lengths . (0.0 1.5 1.25 1.0))
		
;;  Stems in unnatural (forced) direction should be shortened,
;;  according to [Roush & Gourlay].  Their suggestion to knock off
;;  a whole staffspace seems a bit drastical: we'll do half.

		(lengths . (3.5 3.5 3.5 4.5 5.0))
		(stem-shorten . (0.5))
		; if stem is on middle line, choose this direction.
		(default-neutral-direction . 1)
		(X-offset-callbacks . (,Stem::off_callback))		
		(meta . ,(element-description  "Stem" stem-interface ))
	))

	(StemTremolo . (
	   	(molecule-callback . ,Stem_tremolo::brew_molecule)
		(beam-width . 2.0) ; staff-space
		(beam-thickness . 0.42) ; staff-space
		(beam-space-function . ,default-beam-space-function)
		(meta . ,(element-description "StemTremolo" stem-tremolo-interface ))
	))

	(SeparationItem . (
		(meta . ,(element-description "SeparationItem" separation-item-interface ))
	))
	(SeparatingGroupSpanner . (
		(spacing-procedure . ,Separating_group_spanner::set_spacing_rods)
		(meta . ,(element-description "SeparatingGroupSpanner" separation-spanner-interface))
	))

	(SustainPedal . (
		(no-spacing-rods . #t)
		(molecule-callback . ,Sustain_pedal::brew_molecule)
		(self-alignment-X . 0)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))

		(meta . ,(element-description "SustainPedal" sustain-pedal-interface side-position-interface))
	))

	(SystemStartDelimiter . (
		(molecule-callback . ,System_start_delimiter::brew_molecule)
		(after-line-breaking-callback . ,System_start_delimiter::after_line_breaking)
		(collapse-height . 1.0)
		(thickness . 1.6)
		(arch-height . 1.5)
		(arch-angle . 50.0)
		(arch-thick . 0.25)
		(glyph . bar-line)
		(arch-width . 1.5)
		(bracket-thick . 0.25)
		(bracket-width . 2.0)
		(Y-extent-callback . #f)
		(meta . ,(element-description "SystemStartDelimiter" system-start-delimiter ))
	))

	(TextScript . (
		(molecule-callback . ,Text_item::brew_molecule)
		(no-spacing-rods . #t)
		(padding . 0.5)
               (properties-to-font-name . ,properties-to-font-name)
               (style-to-font-name . ,style-to-font-name)
               (markup-to-properties . ,markup-to-properties)
               (font-family . "roman")
		(meta . ,(element-description "TextScript" text-script-interface text-interface side-position-interface ))
	))
	(TextSpanner . (
		(molecule-callback . ,Text_spanner::brew_molecule)
                (font-shape . "italic")
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		(type . "line")
		(direction . 1)
		(meta . ,(element-description "TextSpanner" text-spanner-interface ))		
	))
	(Tie . (
		(molecule-callback . ,Tie::brew_molecule)
		(spacing-procedure . ,Tie::set_spacing_rods)
		(staffline-clearance . 0.35)
		(details . ((ratio . 0.333) (height-limit . 1.0)))
		(thickness . 1.2)
		(x-gap . 0.2)
		(minimum-length  . 2.5)
		(meta . ,(element-description "Tie" tie-interface ))
	))

	(TieColumn . (
		(after-line-breaking-callback . ,Tie_column::after_line_breaking)
		(meta . ,(element-description "TieColumn" tie-column-interface ))
	))

	(TimeSignature . (
		(molecule-callback . ,Time_signature::brew_molecule)
		(break-align-symbol . Time_signature)
		(visibility-lambda . ,all-visible)
		(breakable . #t)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
                (font-style . timesig)
		
		(meta . ,(element-description "TimeSignature" time-signature-interface ))
	))

	(TupletBracket . (
		(number-gap . 2.0)   
		(delta-y . 0)
		(thick . 1.0)
		(after-line-breaking-callback . ,Tuplet_spanner::after_line_breaking)
		(molecule-callback . ,Tuplet_spanner::brew_molecule)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
                (font-shape . "italic")
		(meta .  ,(element-description "TupletBracket"
			   tuplet-bracket-interface))
	))

	(UnaChordaPdeal . (
		(molecule-callback . ,Text_item::brew_molecule)
                (font-shape . "italic")
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
		(no-spacing-rods . #t)
		(self-alignment-X . 0)
		(X-offset-callbacks . (,Side_position::aligned_on_self))
		(Y-offset-callbacks .
		 (,Side_position::aligned_side
		  ,Side_position::centered_on_parent))
		(meta . ,(element-description "UnaChordaPedal" text-interface ))
	))

	(VoltaBracket . (
		(molecule-callback . ,Volta_spanner::brew_molecule)
		(direction . 1)
		(padding . 5)
                (properties-to-font-name . ,properties-to-font-name)
                (style-to-font-name . ,style-to-font-name)
                (markup-to-properties . ,markup-to-properties)
                (font-style . volta)
	
		(thickness . 1.6)  ;  stafflinethickness
		(height . 2.0) ; staffspace;
		(minimum-space . 25)
		(meta . ,(element-description "VoltaBracket" volta-bracket-interface side-position-interface))
	))

	(VerticalAlignment . (
		(axes 1)
		(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
		(X-extent-callback . #f)
		(stacking-dir . -1)
		(meta . ,(element-description "VerticalAlignment" align-interface axis-group-interface))
	))

	(VerticalAxisGroup . (
		(axes 1)
		(meta . ,(element-description "VerticalAxisGroup" axis-group-interface))
	))
))



;  (display  (map pair? all-element-descriptions))

