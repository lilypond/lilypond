;;;; grob-description.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--20.301  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

					; distances are given in stafflinethickness (thicknesses) and
					; staffspace (distances)

;;; WARNING: the meta field should be the last one.

;; TODO: junk the meta field in favor of something more compact?
(define all-grob-descriptions
  `(
    (Accidentals
     . (
	(molecule-callback . ,Local_key_item::brew_molecule)
	(X-offset-callbacks . (,Side_position_interface::aligned_side))
	(after-line-breaking-callback . ,Local_key_item::after_line_breaking)
	(direction . -1)
	(left-padding . 0.2)
	(right-padding . 0.4)
	(paren-cautionaries . #t)
	(font-family . music)
	(meta . ,(grob-description accidentals-interface font-interface side-position-interface))
	))

    (Arpeggio
     . (
	(X-extent-callback . ,Arpeggio::width_callback)
	(Y-extent-callback . #f)	       
	(molecule-callback . ,Arpeggio::brew_molecule)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(X-offset-callbacks . (,Side_position_interface::aligned_side))
	(direction . -1)
	(staff-position . 0.0)
	(meta . ,(grob-description arpeggio-interface side-position-interface font-interface))
	))

    (BarLine
     . (
	(break-align-symbol . staff-bar)
	(glyph . "|")
	(break-glyph-function . ,default-break-barline)
	(bar-size-procedure . ,Bar_line::get_staff_bar_size)
	(molecule-callback . ,Bar_line::brew_molecule)	   
	(visibility-lambda . ,all-visible)
	(breakable . #t)
	(before-line-breaking-callback . ,Bar_line::before_line_breaking)
	(space-alist . (
			(time-signature . (extra-space . 0.75)) 
			(custos . (minimum-space . 2.0))
			(clef .   (minimum-space . 1.0))
			(begin-of-note . (extra-space . 1.0))
			))

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;; 
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.6)
	(thick-thickness . 6.0)
	(meta . ,(grob-description bar-line-interface font-interface))
	))

    
    (BarNumber
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(breakable . #t)
	(visibility-lambda . ,begin-of-line-visible)
	(padding . 1.0)
	(direction . 1)
	(font-family . roman)
	(font-relative-size . -1)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(meta . ,(grob-description
		  side-position-interface
		  text-interface  font-interface break-aligned-interface))
	))

    (BassFigure
     . (
	(molecule-callback . ,brew-bass-figure)
	(Y-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(direction . 0)
	(font-family . number)
	(font-relative-size . -1)
	(padding . 0.1)
	(kern . 0.2)
	(thickness . 1.0)
	(meta . ,(grob-description text-interface font-interface ))
	))
    (Beam
     . (
	;; todo: clean this up a bit: the list is getting
	;; rather long.
	(molecule-callback . ,Beam::brew_molecule)
	(y-dy-callbacks . (,Beam::least_squares
			   ,Beam::cancel_suspect_slope
			   ,Beam::slope_damping
			   ,Beam::quantise_dy
			   ,Beam::user_override
			   ,Beam::do_quantise_y))

	(thickness . 0.48) ; in staff-space
	(before-line-breaking-callback . ,Beam::before_line_breaking)
	(after-line-breaking-callback . ,Beam::after_line_breaking)
	(neutral-direction . -1)
	(dir-function . ,beam-dir-majority)
	(height-quants .  ,default-beam-dy-quants)
	(vertical-position-quant-function . ,default-beam-y-quants)
	(beamed-stem-shorten . (0.5))
	(outer-stem-length-limit . 0.2)
	(slope-limit . 0.2)
	(flag-width-function . ,default-beam-flag-width-function)
	(space-function . ,default-beam-space-function)
	(damping . 1)
	(auto-knee-gap . 7)
	(meta . ,(grob-description beam-interface))
	))

    (BreakAlignment
     . (
	(breakable . #t)
	(stacking-dir . 1)
	(axes 0)
	(meta . ,(grob-description
		  axis-group-interface 
		  )
	      )
	))

    (BreakAlignGroup
     . (
	(axes  . (0))
	(X-offset-callbacks . (,Break_align_interface::alignment_callback))

	(meta . ,(grob-description axis-group-interface))
	))

    (BreathingSign
     . (
	(break-align-symbol . breathing-sign)
	(breakable . #t )
	(space-alist . (
			(key-signature . (minimum-space . 1.5))
			(staff-bar . (minimum-space . 1.5))
			(clef . (minimum-space . 2.0))
			(begin-of-note . (minimum-space . 1.0))
			))
	(molecule-callback . ,Text_item::brew_molecule)
	(lookup . name)
	(font-family . music)
	(text . "scripts-rcomma")
	(Y-offset-callbacks . (,Breathing_sign::offset_callback))
	(visibility-lambda . ,begin-of-line-invisible)
	(meta . ,(grob-description  break-aligned-interface text-interface font-interface))
	))

    (Clef
     . (
	(molecule-callback . ,Clef::brew_molecule)
	(before-line-breaking-callback . ,Clef::before_line_breaking)
	(breakable . #t)
	(font-family . music)	   
	(break-align-symbol . clef)
	(visibility-lambda . ,begin-of-line-visible)
	(space-alist . (
			(staff-bar . (minimum-space .  3.7))
			(key-signature . (minimum-space . 4.0))
			(time-signature . (minimum-space . 4.2))
			(begin-of-note . (minimum-space . 5.0))
			))
	(Y-offset-callbacks  . (,Staff_symbol_referencer::callback)) 
	(meta . ,(grob-description clef-interface font-interface break-aligned-interface ))
	))

    (ChordName
     . (
	(molecule-callback . ,Chord_name::brew_molecule)
	(after-line-breaking-callback . ,Chord_name::after_line_breaking)
	(chord-name-function . ,default-chord-name-function)
	(font-family . roman)
	(meta . ,(grob-description  font-interface text-interface chord-name-interface))
	))

    (Custos
     . (
	(break-align-symbol . custos)
	(breakable . #t)
	(molecule-callback . ,Custos::brew_molecule)
	(visibility-lambda . ,end-of-line-visible)
	(style . vaticana)
	(neutral-position . 0)
	(neutral-direction . -1)
	(adjust-if-on-staffline . #t)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(font-family . music)
	(space-alist . (
			(begin-of-note . (minimum-space . 0.0))
			))
	(meta . ,(grob-description custos-interface staff-symbol-referencer-interface break-aligned-interface) )
	))


    (DotColumn
     . (
	(axes 0)
	(direction . 1)
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)

	(X-offset-callbacks . (,Dot_column::side_position))
	(meta . ,(grob-description dot-column-interface axis-group-interface))
	))

    (Dots
     . (
	(molecule-callback . ,Dots::brew_molecule)
	(dot-count . 1)
	(staff-position . 0.0)
	(Y-offset-callbacks  . (,Dots::quantised_position_callback ,Staff_symbol_referencer::callback))
	(meta . ,(grob-description  font-interface dots-interface ))
	))

    (DoublePercentRepeat .
			 (
			  (molecule-callback . ,Percent_repeat_item_interface::double_percent)
			  (breakable . #t)
			  (slope . 1.0)
			  (font-family . music)
			  (width . 2.0)
			  (thickness . 0.48)
			  (break-align-symbol . staff-bar)
			  (visibility-lambda . ,begin-of-line-invisible)
			  (meta . ,(grob-description font-interface percent-repeat-interface))
			  ))

    (DynamicText
     . (
	(Y-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(molecule-callback . ,Text_item::brew_molecule)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(self-alignment-X . 0)

	(no-spacing-rods . #t)
	(script-priority . 100)
	(font-series . bold)
	(font-family . dynamic)
	(font-shape . italic)
	(self-alignment-Y . 0)
	(meta . ,(grob-description font-interface text-interface dynamic-interface))
	))

    (DynamicLineSpanner
     . (
	(axes . ( 1))
	(padding . 0.6)
	(minimum-space . 1.2)
	(direction . -1)
	(meta . ,(grob-description dynamic-interface axis-group-interface side-position-interface))
	))

    (LeftEdge
     . (
	(break-align-symbol . left-edge)
	(X-offset-callbacks . (,Break_align_interface::alignment_callback))
	(X-extent-callback . ,Grob::point_dimension_callback)
	(breakable . #t)
	(space-alist . (
			(time-signature . (extra-space . 0.0)) 
			(staff-bar . (extra-space . 0.0))
			(breathing-sign . (minimum-space  . 0.0))
			(clef . (extra-space . 1.0))
			(begin-of-note . (extra-space . 0.0))
			(key-signature . (extra-space . 0.0))
			))
	(meta . ,(grob-description break-aligned-interface))
	))

    (Fingering
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(X-offset-callbacks . (,Side_position_interface::centered_on_parent
			       ,Side_position_interface::aligned_on_self))
	(padding . 0.6)
					;		(direction . -1)
	(self-alignment-X . 0)
	(self-alignment-Y . 0)
	(font-family . number)
	(font-relative-size . -3)
	(font-shape . upright)
	(meta . ,(grob-description finger-interface  font-interface
				   text-script-interface text-interface side-position-interface))
	))


    (HaraKiriVerticalGroup
     . (
	(Y-offset-callbacks . (,Hara_kiri_group_spanner::force_hara_kiri_callback))
	(Y-extent-callback . ,Hara_kiri_group_spanner::y_extent)
	(axes 1)
	(meta . ,(grob-description axis-group-interface hara-kiri-group-interface))
	))

    (Hairpin
     . (
	(molecule-callback . ,Hairpin::brew_molecule)
	(thickness . 1.0)
	(height . 0.6666)
	(spacing-procedure . ,Spanner::set_spacing_rods)
	(minimum-length . 2.0)
	(if-text-padding . 1.0)
	(width-correct . -1.0)

	(dash-thickness . 1.2)
	(dash-length . 4.0)
	(self-alignment-Y . 0)
	(Y-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(meta . ,(grob-description hairpin-interface dynamic-interface))
	))

    (InstrumentName
     . (
	(breakable . #t)
	(Y-offset-callbacks . (,Side_position_interface::aligned_on_self
			       ,Side_position_interface::aligned_on_support_refpoints))
	;; huh? what's this for?
	(direction . 0)
	(space-alist . (
			(left-edge . (extra-space . 1.0))
			))

	(self-alignment-Y . 0)
	(molecule-callback . ,Text_item::brew_molecule)		
	(break-align-symbol . instrument-name)
	(visibility-lambda . ,begin-of-line-visible)
	(baseline-skip . 2)
	(font-family . roman)
	(meta . ,(grob-description  font-interface  text-interface break-aligned-interface))
	))

    (KeySignature
     . (
	(molecule-callback . ,Key_signature_interface::brew_molecule)
	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar .  (extra-space . 1.1))
			(begin-of-note . (extra-space . 2.5))
			))
	(break-align-symbol . key-signature)
	(visibility-lambda . ,begin-of-line-visible)
	(breakable . #t)
	(meta . ,(grob-description key-signature-interface  font-interface  break-aligned-interface))
	))


    (LyricHyphen
     . (
	(thickness . 1.0)
	(height . 0.4)
	(minimum-length .  0.5) 
	(maximum-length .  100)
	(molecule-callback . ,Hyphen_spanner::brew_molecule)
	(Y-extent-callback . ,Grob::point_dimension_callback)
	(meta . ,(grob-description lyric-hyphen-interface ))
	))

    (LineOfScore
     . (
	(axes . (0 1))
	(meta . ,(grob-description  line-of-score-interface axis-group-interface))
	))

    (LyricExtender
     . (
	(molecule-callback . ,Lyric_extender::brew_molecule)
	(height . 0.8) ; stafflinethickness;
	(right-trim-amount . 0.5)
	(Y-extent-callback . ,Grob::point_dimension_callback)
	(meta . ,(grob-description  lyric-extender-interface))
	))

    (LyricText
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(self-alignment-X . 0)
	(non-rhythmic . #t)
	(word-space . 0.6)
	(ignore-length-mismatch . #f)
	(begin-alignment . 4)
	(end-alignment . 2)
	(font-family . roman)
	(font-shape . upright)
	(meta . ,(grob-description lyric-syllable-interface text-interface font-interface ))
	))

    (Porrectus
     . (
	(style . mensural)
	(auto-properties . #f)
	(solid . #f)
	(porrectus-width . 2.4)
	(line-thickness . 1.0)
	(add-stem . #t)
	(stem-direction . 1)
	(molecule-callback . ,Porrectus::brew_molecule)
	(meta . ,(grob-description porrectus-interface))
	))

    (RehearsalMark
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(self-alignment-X . 0)

	(direction . 1)
	(breakable . #t)
	(font-family . roman)
	(font-shape . upright)
	(font-relative-size . 1)
	(visibility-lambda . ,end-of-line-invisible)
	(padding . 0.8)
	(meta . ,(grob-description  mark-interface side-position-interface))
	))

    (MultiMeasureRest
     . (
	(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
	(molecule-callback . ,Multi_measure_rest::brew_molecule)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(staff-position . 0)
	(expand-limit . 10)
	(padding . 2.0) ; staffspace
	(minimum-width . 12.5) ; staffspace
	(font-family . number)
	(font-relative-size . 1)
	(meta . ,(grob-description multi-measure-rest-interface rest-interface font-interface ))
	))

    (NoteCollision
     . (
	(axes 0 1)
	;; Ugh, should not be hard-coded. 
	(note-width . 1.321)
	(meta . ,(grob-description
		  note-collision-interface axis-group-interface
		  ))
	))

    (NoteColumn
     . (
	(axes . (0 1))
	(meta . ,(grob-description axis-group-interface note-column-interface))
	))

    (NoteHead
     . (
	(style . default)
	(molecule-callback . ,Note_head::brew_molecule)
	(Y-offset-callbacks  . (,Staff_symbol_referencer::callback))
	(stem-attachment-function . ,note-head-style->attachment-coordinates)
	(meta . ,(grob-description rhythmic-head-interface font-interface note-head-interface))
	))

    (Glissando
     . (
	(type . line)
	(gap . 0.5)
	(breakable . #t)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)			 
	(molecule-callback . ,Line_spanner::brew_molecule)
	(meta . ,(grob-description line-spanner-interface))
	))

    (VoiceFollower
     . (
	(type . line)
	(gap . 0.5)
	(breakable . #t)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)			 
	(molecule-callback . ,Line_spanner::brew_molecule)
	(meta . ,(grob-description line-spanner-interface))
	))

    (NoteName
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(font-family . roman)
	(meta . ,(grob-description note-name-interface font-interface))
	))

    (OctavateEight
     . (
	(self-alignment-X . 0)
	(text . "8")
	(visibility-lambda . ,begin-of-line-visible)
	(X-offset-callbacks . (,Side_position_interface::centered_on_parent ,Side_position_interface::aligned_on_self))
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(molecule-callback . ,Text_item::brew_molecule)
	(font-shape . italic)
	(font-family . roman)
	(meta . ,(grob-description text-interface font-interface ))
	))

    (PaperColumn
     . (
	(axes 0)
					;	        (molecule-callback . ,Paper_column::brew_molecule) (font-name . "cmr8")
	(meta . ,(grob-description paper-column-interface axis-group-interface spaceable-element-interface))
	))

    (PhrasingSlur
     . (
	(molecule-callback . ,Slur::brew_molecule)
	(thickness . 1.2)		
	(spacing-procedure . ,Spanner::set_spacing_rods)		
	(minimum-length . 1.5)
	(after-line-breaking-callback . ,Slur::after_line_breaking)
	(extremity-rules . ,default-slur-extremity-rules)
	(extremity-offset-alist . ,default-phrasing-slur-extremity-offset-alist)
	(de-uglify-parameters . ( 1.5  0.8  -2.0))
	(Y-extent-callback . ,Slur::height)
	(details . ((height-limit . 2.0) (ratio . 0.333) (force-blowfit . 0.5)
		    (bezier-pct-c0 . -0.2) (bezier-pct-c3 . 0.000006)
		    (bezier-pct-out-max . 0.8) (bezier-pct-in-max . 1.2)
		    (bezier-area-steps . 1.0)))
	(beautiful . 0.5)
	(y-free . 0.75)
	(attachment . (#f . #f))
	(attachment-offset . ((0 . 0) . (0 . 0)))
	(slope-limit . 0.8)
	(meta . ,(grob-description slur-interface))
	))

    (NonMusicalPaperColumn
     . (
	(axes 0)
					;	(molecule-callback . ,Paper_column::brew_molecule) (font-name . "cmr8")
	(meta . ,(grob-description paper-column-interface
				   axis-group-interface spaceable-element-interface))
	))

    (PercentRepeat
     . (
	(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
	(molecule-callback . ,Multi_measure_rest::percent)
	(slope . 1.0)
	(thickness . 0.48)
	(minimum-width . 12.5) ; staffspace
	(font-family . music)
	(meta . ,(grob-description multi-measure-rest-interface  font-interface percent-repeat-interface))
	))

    (RepeatSlash
     . (
	(molecule-callback . , Percent_repeat_item_interface::beat_slash)
	(thickness . 0.48)
	(slope . 1.7)
	(meta . ,(grob-description percent-repeat-interface))
	))
    (Rest
     . (
	(after-line-breaking-callback . ,Rest::after_line_breaking)
	(X-extent-callback . ,Rest::extent_callback)
	(Y-extent-callback . ,Rest::extent_callback)		
	(molecule-callback . ,Rest::brew_molecule)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback)) 
	(minimum-beam-collision-distance . 1.5)
	(meta . ,(grob-description  
		  rhythmic-head-interface
		  staff-symbol-referencer-interface
		  rest-interface))
	))

    (RestCollision
     . (
	(minimum-distance . 0.75)
	(meta . ,(grob-description rest-collision-interface ))
	))

    (Script
     . (
	;; don't set direction here: it breaks staccato.
	(molecule-callback . ,Script::brew_molecule)
	(padding . 0.29) 
	(X-offset-callbacks . (,Side_position_interface::centered_on_parent))
	(before-line-breaking-callback . ,Script::before_line_breaking)
	(font-family . music)
	(meta . ,(grob-description script-interface side-position-interface font-interface))
	))

    (ScriptColumn
     . (
	(before-line-breaking-callback . ,Script_column::before_line_breaking)
	(meta . ,(grob-description script-column-interface))
	))

    (Slur
     . (
	(molecule-callback . ,Slur::brew_molecule)
	(thickness . 1.2)		
	(spacing-procedure . ,Spanner::set_spacing_rods)		
	(minimum-length . 1.5)
	(after-line-breaking-callback . ,Slur::after_line_breaking)
	(extremity-rules . ,default-slur-extremity-rules)
	(extremity-offset-alist . ,default-slur-extremity-offset-alist)
	(de-uglify-parameters . ( 1.5  0.8  -2.0))
	(Y-extent-callback . ,Slur::height)
	(details . ((height-limit . 2.0) (ratio . 0.333) (force-blowfit . 0.5)
		    (bezier-pct-c0 . -0.2) (bezier-pct-c3 . 0.000006)
		    (bezier-pct-out-max . 0.8) (bezier-pct-in-max . 1.2)
		    (bezier-area-steps . 1.0)))
	(beautiful . 0.5)
	(y-free . 0.75)
	(attachment . (#f . #f))
	(attachment-offset . ((0 . 0) . (0 . 0)))
	(slope-limit . 0.8)
	(meta . ,(grob-description slur-interface))
	))

    (SpacingSpanner
     . (
	(spacing-procedure .  ,Spacing_spanner::set_springs)
	(grace-space-factor . 0.8)

	;; TODO: change naming -- unintuitive
	(arithmetic-basicspace . 2.0)
	(arithmetic-multiplier . ,(* 0.9 1.32))
	(X-extent-callback . #f)
	(Y-extent-callback . #f)
	;; assume that notes at least this long are present.
	(maximum-duration-for-spacing . ,(make-moment 1 8))
	(meta . ,(grob-description  spacing-spanner-interface))
	))

    (SpanBar
     . (
	(break-align-symbol . staff-bar)
	(bar-size-procedure . ,Span_bar::get_bar_size) 
	(molecule-callback . ,Span_bar::brew_molecule)
	(visibility-lambda . ,begin-of-line-invisible)
	(X-extent-callback . ,Span_bar::width_callback)
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
	(meta . ,(grob-description span-bar-interface bar-line-interface ))
	))

    (StanzaNumber
     . (
	(breakable . #t)
	(molecule-callback . ,Text_item::brew_molecule)		
	(break-align-symbol . clef)
	(visibility-lambda . ,begin-of-line-visible)
	(font-family . roman)
	(meta . ,(grob-description break-aligned-interface text-interface font-interface))		
	))

    (StaffSpacing
     . (
	(breakable . #t)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)
	(meta . ,(grob-description staff-spacing-interface))
	))
    (NoteSpacing
     . (
	(X-extent-callback . #f)
	(Y-extent-callback . #f)
	(stem-spacing-correction . 0.5)
	(space-factor . 1.0)
	(meta . ,(grob-description note-spacing-interface))
	))

    (StaffSymbol
     . (
	(molecule-callback . ,Staff_symbol::brew_molecule)
	(staff-space . 1.0)
	(line-count . 5)
	(layer . 0)
	(meta . ,(grob-description staff-symbol-interface ))
	))

    (SostenutoPedal
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(direction . -1)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(Y-offset-callbacks .
			    (,Side_position_interface::aligned_side
			     ,Side_position_interface::centered_on_parent))
	(no-spacing-rods . #t)
	(font-shape . italic)
	(self-alignment-X . 0)
	(meta . ,(grob-description text-interface  font-interface))
	))

    (Stem
     . (
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
	(neutral-direction . -1)
	(X-offset-callbacks . (,Stem::off_callback))
	(X-extent-callback . ,Stem::dim_callback)	
	(Y-extent-callback . ,Stem::height)
	(adjust-if-on-staffline . #t)
	(font-family . music)	   
	(meta . ,(grob-description stem-interface  font-interface))
	))

    (StemTremolo
     . (
	(molecule-callback . ,Stem_tremolo::brew_molecule)
	(Y-extent-callback . ,Stem_tremolo::height)
	(X-extent-callback . #f)

	(beam-width . 2.0) ; staff-space
	(beam-thickness . 0.42) ; staff-space
	(beam-space-function . ,default-beam-space-function)
	(meta . ,(grob-description stem-tremolo-interface ))
	))

    (SeparationItem
     . (
	(meta . ,(grob-description separation-item-interface ))
	))

    (SeparatingGroupSpanner
     . (
	(spacing-procedure . ,Separating_group_spanner::set_spacing_rods)
	(meta . ,(grob-description separation-spanner-interface))
	))

    (SustainPedal
     . (
	(no-spacing-rods . #t)
	(molecule-callback . ,Sustain_pedal::brew_molecule)
	(self-alignment-X . 0)
	(direction . -1)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(Y-offset-callbacks .
			    (,Side_position_interface::aligned_side
			     ,Side_position_interface::centered_on_parent))

	(meta . ,(grob-description sustain-pedal-interface side-position-interface font-interface))
	))

    (SystemStartBrace
     . (
	(glyph . "brace")
	(molecule-callback . ,System_start_delimiter::brew_molecule)
	(collapse-height . 5.0)
	(font-family . braces)
	(Y-extent-callback . #f)
	(meta . ,(grob-description system-start-delimiter-interface font-interface))
	))

    (SystemStartBracket
     . (
	(Y-extent-callback . #f)
	(molecule-callback . ,System_start_delimiter::brew_molecule)
	(glyph . "bracket")
	(arch-height . 1.5)
	(arch-angle . 50.0)
	(arch-thick . 0.25)
	(arch-width . 1.5)
	(bracket-collapse-height . 1)
	(thickness . 0.25)
	(meta . ,(grob-description system-start-delimiter-interface ))
	))

    (SystemStartBar
     . (
	(Y-extent-callback . #f)
	(molecule-callback . ,System_start_delimiter::brew_molecule)
	(glyph . "bar-line")
	(thickness . 1.6)
	(after-line-breaking-callback . ,System_start_delimiter::after_line_breaking)
	(meta . ,(grob-description system-start-delimiter-interface ))
	))

    (TextScript
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(no-spacing-rods . #t)
	(direction . -1)
	(padding . 0.5)
	;; todo: add X self alignment?
	(baseline-skip . 2)
	(font-family . roman)
	(meta . ,(grob-description text-script-interface text-interface side-position-interface font-interface ))
	))

    (TextSpanner
     . (
	(molecule-callback . ,Text_spanner::brew_molecule)
	(font-family . roman)
	(type . "line")

	;; urg, only for (de)cresc. text spanners
	(if-text-padding . 1.0)
	(width-correct . -1)

	(direction . 1)
	(meta . ,(grob-description text-spanner-interface  font-interface))		
	))

    (Tie
     . (
	(molecule-callback . ,Tie::brew_molecule)
	(spacing-procedure . ,Spanner::set_spacing_rods)
	(staffline-clearance . 0.35)
	(details . ((ratio . 0.333) (height-limit . 1.0)))
	(thickness . 1.2)
	(x-gap . 0.2)
	(y-offset . 0.6)
	(minimum-length  . 2.5)
	(meta . ,(grob-description tie-interface ))
	))

    (TieColumn
     . (
	(after-line-breaking-callback . ,Tie_column::after_line_breaking)
	(meta . ,(grob-description tie-column-interface ))
	))

    (TimeSignature
     . (
	(molecule-callback . ,Time_signature::brew_molecule)
	(break-align-symbol . time-signature)
	(visibility-lambda . ,all-visible)
	(space-alist . (
			(begin-of-note . (extra-space . 2.0))
			(staff-bar .  (minimum-space . 2.0))
			))
	(breakable . #t)
	(style . C)
	(font-family . number)
	(meta . ,(grob-description time-signature-interface  font-interface))
	))

    (TupletBracket
     . (
	(number-gap . 2.0)   
	(thick . 1.0)
	(after-line-breaking-callback . ,Tuplet_bracket::after_line_breaking)
	(molecule-callback . ,Tuplet_bracket::brew_molecule)
	(font-family . roman)
	(font-shape . italic)
	(font-relative-size . -1)
	(meta .  ,(grob-description text-interface
				    tuplet-bracket-interface font-interface))
	))

    (UnaCordaPedal
     . (
	(molecule-callback . ,Text_item::brew_molecule)
	(font-family . roman)
	(font-shape . italic)
	(no-spacing-rods . #t)
	(self-alignment-X . 0)
	(direction . -1)
	(X-offset-callbacks . (,Side_position_interface::aligned_on_self))
	(Y-offset-callbacks .
			    (,Side_position_interface::aligned_side
			     ,Side_position_interface::centered_on_parent))
	(meta . ,(grob-description text-interface font-interface))
	))

    (VoltaBracket
     . (
	(molecule-callback . ,Volta_spanner::brew_molecule)
	(direction . 1)
	(padding . 1)
	(font-style . volta)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(thickness . 1.6)  ;  stafflinethickness
	(height . 2.0) ; staffspace;
	(minimum-space . 5)
	(font-family . number)
	(font-relative-size . -2)
	(meta . ,(grob-description volta-bracket-interface side-position-interface font-interface))
	))
    
    (VerticalAlignment
     . (
	(axes 1)
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
	(X-extent-callback . #f)
	(stacking-dir . -1)
	(meta . ,(grob-description align-interface axis-group-interface))
	))

    (VerticalAxisGroup
     . (
	(axes 1)
	(meta . ,(grob-description axis-group-interface))
	))
    )
  )




(define (completize-grob-entry x)
  "transplant assoc key into 'name entry of 'meta of X
"
  (let* ((name-sym  (car x))
	 (entry (cdr x))
	 (metaentry (cdr (assoc 'meta entry)))
	 (new-metaentry (assoc-set! metaentry 'name name-sym))
	 )

    (cons name-sym (assoc-set! entry 'meta new-metaentry))
    ))

(set! all-grob-descriptions (map completize-grob-entry all-grob-descriptions))



					;  (display  (map pair? all-grob-descriptions))


;; make sure that \property Foo.Bar =\turnOff doesn't complain

(map (lambda (x)
					; (display (car x)) (newline)

       (set-object-property! (car x) 'translation-type? list?))
     all-grob-descriptions)

