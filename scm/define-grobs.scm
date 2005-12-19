;;;; define-grobs.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; distances are given in linethickness (thicknesses) and
;;;; staffspace (distances)

;;;; WARNING: the meta field should be the last one.
;;;; WARNING: don't use anonymous functions for initialization.

;; TODO: junk the meta field in favor of something more compact?

;;; todo:: reorder sensibly.

(define-public all-grob-descriptions
  `(
    (Accidental
     . (
	(avoid-slur . inside)
	(cautionary-style . parentheses)

	(stencil . ,Accidental_interface::print)
	(after-line-breaking
	 . ,Accidental_interface::after_line_breaking)
					
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface))))))
    
    (AccidentalSuggestion
     . (
	(stencil . ,Accidental_interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure (list Self_alignment_interface::centered_on_x_parent))
			,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self)))))
	(self-alignment-X . ,CENTER)
	(cautionary . #t)
	(cautionary-style . smaller)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(cautionary-style . parentheses)
	(direction . ,UP)
	(staff-padding . 0.25)
	(script-priority . 0)
	(side-axis . ,X)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				script-interface
				accidental-interface
				accidental-suggestion-interface
				self-alignment-interface
				font-interface))))))
    (AccidentalPlacement
     . (
	(left-padding . 0.2)

	(positioning-done . ,Accidental_placement::calc_positioning_done)
	(X-extent . ,Axis_group_interface::width)		      
	
	;; this is quite small, but it is very ugly to have
	;; accs closer to the previous note than to the next one.
	(right-padding . 0.15)
	(meta . ((class . Item)
		 (interfaces . (accidental-placement-interface))))))
    (Ambitus
     . (
	(axes . (0 1))
	(X-extent . ,Axis_group_interface::width)
	(X-extent . ,Axis_group_interface::height)

	(space-alist . (
			(clef . (extra-space . 0.5))
			(key-signature . (extra-space . 0.0))
			(staff-bar . (extra-space . 0.0))
			(time-signature . (extra-space . 0.0))
			(first-note . (fixed-space . 0.0))))
	(breakable . #t)
	(break-align-symbol . ambitus)
	(break-visibility . ,begin-of-line-visible)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				break-aligned-interface
				ambitus-interface))))))

    (AmbitusLine
     . (

	(stencil . ,Ambitus::print)

	(join-heads . #t)
	(thickness . 2)
	(X-offset . ,Self_alignment_interface::centered_on_x_parent)

	(meta . ((class . Item)
		 (interfaces . (ambitus-interface
				staff-symbol-referencer-interface
				font-interface))))))
    (AmbitusAccidental
     . (
	(font-family . music)
	(padding . 0.5)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . -1)
	(cautionary-style . parentheses)

	(stencil . ,Accidental_interface::print)
	(after-line-breaking . ,Accidental_interface::after_line_breaking)
	(side-axis . ,X)
	
	(meta . ((class . Item)
		 (interfaces . (item-interface
				accidental-interface
				break-aligned-interface
				side-position-interface
				font-interface))))))

    (AmbitusNoteHead
     . (
	(duration-log . 2)

	(stencil . ,Note_head::print)
	(glyph-name . ,note-head::calc-glyph-name)
	
	(Y-offset . ,Staff_symbol_referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				note-head-interface
				ambitus-interface
				staff-symbol-referencer-interface
				rhythmic-head-interface
				ledgered-interface))))))

    (Arpeggio
     . ((X-extent . ,Arpeggio::width)
	(stencil . ,Arpeggio::print)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . -1)
	(padding . 0.5)
	(side-axis . ,X)
	(staff-position . 0.0)
	(meta . ((class . Item)
		 (interfaces . (arpeggio-interface
				staff-symbol-referencer-interface
				side-position-interface
				font-interface))))))

    (BarLine
     . (
	(break-align-symbol . staff-bar)
	(glyph . "|")
	(layer . 0)
	(break-visibility . ,all-visible)
	(breakable . #t)

	(stencil . ,Bar_line::print)
	(glyph-name . ,bar-line::calc-glyph-name)
	(bar-size .  ,Bar_line::calc_bar_size)
	
	(space-alist . (
			(time-signature . (extra-space . 0.75))
			(custos . (minimum-space . 2.0))
			(clef . (minimum-space . 1.0))
			(key-signature . (extra-space . 1.0))
			(key-cancellation . (extra-space . 1.0))
			(first-note . (fixed-space . 1.3))
			(next-note . (semi-fixed-space . 1.3))
			(right-edge . (extra-space . 0.0))))

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	;; TODO:
	;; kern should scale with linethickness too.
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.9)
	(thick-thickness . 6.0)
	(meta . ((class . Item)
		 (interfaces . (bar-line-interface
				break-aligned-interface
				font-interface))))))

    (BarNumber
     . (
	(stencil . ,Text_interface::print)
	(breakable . #t)
	(break-visibility . ,begin-of-line-visible)
	(padding . 1.0)
	(direction . 1)
	(font-family . roman)
	(font-size . -2)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(self-alignment-X . 1)

	(meta .
	      ((class . Item)
	       (interfaces . (side-position-interface
			      text-interface
			      self-alignment-interface
			      font-interface
			      break-aligned-interface))))
	))

    (BassFigure
     . (
	;
	(stencil . ,Text_interface::print)

	(meta . ((class . Item)
		 (interfaces . (text-interface
				rhythmic-grob-interface
				bass-figure-interface
				font-interface))))))
    (BassFigureBracket
     . (
	;
	(stencil . ,Enclosing_bracket::print)

	(edge-height . (0.2 . 0.2))
	(meta . ((class . Item)
		 (interfaces . (enclosing-bracket-interface)) ))
	))
    (BassFigureContinuation
     . (

	(stencil . ,Figured_bass_continuation::print)

	(Y-offset . ,Figured_bass_continuation::center_on_figures)
	(meta . ((class . Spanner)
		 (interfaces . (figured-bass-continuation-interface))
		 ))))
    (BassFigureLine
     . (
	(axes . (,Y))

	(Y-extent . ,Axis_group_interface::height)

	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				))))))

    (BassFigureAlignment
     . (
	(axes . (,Y))
	(threshold . (2 . 1000))

	(positioning-done . ,Align_interface::calc_positioning_done)
	(Y-extent . ,Axis_group_interface::height)
					
	(stacking-dir . -1)
	(meta . ((class . Spanner)
		 (interfaces . (align-interface
				axis-group-interface))))))

    (Beam
     . (
	;; todo: clean this up a bit: the list is getting
	;; rather long.
	(gap . 0.8)
	(positions .  ,(ly:make-simple-closure
			(ly:make-simple-closure
			 (list chain-grob-member-functions
			   `(,cons 0 0)
			   Beam::calc_least_squares_positions
			   Beam::slope_damping
			   Beam::shift_region_to_valid
			   Beam::quanting
			   ))))

	;; this is a hack to set stem lengths, if positions is set.
	(quantized-positions . ,Beam::set_stem_lengths)
	(concaveness . ,Beam::calc_concaveness)
	(direction . ,Beam::calc_direction)
	(shorten . ,Beam::calc_stem_shorten)
	(beaming . ,Beam::calc_beaming)
	(stencil . ,Beam::print)

	;; TODO: should be in SLT.
	(thickness . 0.48) ; in staff-space
	(neutral-direction . -1)

	;; Whe have some unreferenced problems here.
	;;
	;; If we shorten beamed stems less than normal stems (1 staffspace),
	;; or high order less than 8th beams, patterns like
	;;     c''4 [c''8 c''] c''4 [c''16 c]
	;; are ugly (different stem lengths).
	;;
	;; But if we shorten 16th beams as much as 8th beams, a single
	;; forced 16th beam looks *very* short.

	;; We choose to shorten 8th beams the same as single stems,
	;; and high order beams less than 8th beams, so that all
	;; isolated shortened beams look nice and a bit shortened,
	;; sadly possibly breaking patterns with high order beams.
	(beamed-stem-shorten . (1.0 0.5 0.25))
	(damping . 1)
	(auto-knee-gap . 5.5)

	;; only for debugging.
	(font-family . roman)
	(meta . ((class . Spanner)
		 (interfaces . (staff-symbol-referencer-interface
				beam-interface))))))

    (BreakAlignment
     . (
	(breakable . #t)
	(stacking-dir . 1)

	(positioning-done . ,Break_align_interface::calc_positioning_done)
	(X-extent . ,Axis_group_interface::width)
					
	(break-align-orders . ;; end of line
			    #((instrument-name
			       left-edge
			       ambitus
			       breathing-sign
			       clef
			       staff-bar
			       key-cancellation
			       key-signature
			       time-signature
			       custos)

			      ;; unbroken
			      (instrument-name
			       left-edge
			       ambitus
			       breathing-sign
			       clef
			       staff-bar
			       key-cancellation
			       key-signature
			       staff
			       time-signature custos)

			      ;; begin of line
			      (instrument-name
			       left-edge
			       ambitus
			       breathing-sign
			       clef
			       key-cancellation
			       key-signature
			       staff-bar
			       time-signature
			       custos)))
	(axes . (0))
	(meta . ((class . Item)
		 (interfaces . (break-alignment-interface
				axis-group-interface))))))

    (BreakAlignGroup
     . (
	(axes . (0))

	(X-extent . ,Axis_group_interface::width)

	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				axis-group-interface))))))

    (BreathingSign
     . (
	(break-align-symbol . breathing-sign)
	(breakable . #t)
	(space-alist . (
			(ambitus . (extra-space . 2.0))
			(custos . (minimum-space . 1.0))
			(key-signature . (minimum-space . 1.5))
			(time-signature . (minimum-space . 1.5))
			(staff-bar . (minimum-space . 1.5))
			(clef . (minimum-space . 2.0))
			(first-note . (fixed-space . 1.0)) ;huh?
			(right-edge . (extra-space . 0.1))))

	(stencil . ,Text_interface::print)

	(text . ,(make-musicglyph-markup "scripts.rcomma"))
	(Y-offset . ,Breathing_sign::offset_callback)
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				breathing-sign-interface
				text-interface
				font-interface))))))

    (Clef
     . (

	(stencil . ,Clef::print)
	(glyph-name . ,Clef::calc_glyph_name)
					
	(breakable . #t)
	(font-family . music)
	(break-align-symbol . clef)
	(break-visibility . ,begin-of-line-visible)
	(space-alist . ((ambitus . (extra-space . 2.0))
			(staff-bar . (extra-space . 0.7))
			(key-cancellation . (minimum-space . 3.5))
			(key-signature . (minimum-space . 3.5))
			(time-signature . (minimum-space . 4.2))
			(first-note . (minimum-fixed-space . 5.0))
			(next-note . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))))
	(Y-offset . ,Staff_symbol_referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (clef-interface
				staff-symbol-referencer-interface
				font-interface
				break-aligned-interface))))))

    (ClusterSpannerBeacon
     . (

	(Y-extent . ,Cluster_beacon::height)

	(meta . ((class . Item)
		 (interfaces . (cluster-beacon-interface))))))

    (ClusterSpanner
     . (

	(springs-and-rods . ,Spanner::set_spacing_rods)
	(stencil . ,Cluster::print)


	(minimum-length . 0.0)
	(padding . 0.25)
	(style . ramp)
	(meta . ((class . Spanner)
		 (interfaces . (cluster-interface))))))

    (ChordName
     . (

	(stencil . ,Text_interface::print)
	(after-line-breaking . ,Chord_name::after_line_breaking)
	
	(word-space . 0.0)
	(font-family . sans)
	(font-size . 1.5)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				rhythmic-grob-interface
				text-interface
				chord-name-interface
				item-interface))))))

    (CombineTextScript
     . (

	(stencil . ,Text_interface::print)

	(no-spacing-rods . #t)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(direction . 1)
	(padding . 0.5)
	(staff-padding . 0.5)
	(script-priority . 200)
	;; todo: add X self alignment?
	(baseline-skip . 2)
	(side-axis . ,Y)
	(font-series . bold)
	(meta . ((class . Item)
		 (interfaces . (text-script-interface
				text-interface
				side-position-interface
				font-interface))))))

    (Custos
     . (
	(break-align-symbol . custos)
	(breakable . #t)

	(stencil . ,Custos::print)

	(break-visibility . ,end-of-line-visible)
	(style . vaticana)
	(neutral-direction . -1)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(space-alist . (
			(first-note . (minimum-fixed-space . 0.0))
			(right-edge . (extra-space . 0.1))))
	(meta . ((class . Item)
		 (interfaces
		  . (custos-interface
		     staff-symbol-referencer-interface
		     font-interface
		     break-aligned-interface))))))

    (DotColumn
     . (
	(axes . (0))
	(direction . ,RIGHT)

	(positioning-done . ,Dot_column::calc_positioning_done) 
	(X-extent . ,Axis_group_interface::width)
	
	(X-offset . ,Dot_column::side_position)
	(meta . ((class . Item)
		 (interfaces . (dot-column-interface
				axis-group-interface))))))

    (Dots
     . (

	(stencil . ,Dots::print)

	(dot-count . 1)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				staff-symbol-referencer-interface
				dots-interface))))))

    (DoublePercentRepeat
     . (
	(stencil . ,Percent_repeat_item_interface::double_percent)
	(breakable . #t)
	(slope . 1.0)
	(font-encoding . fetaMusic)
	(width . 2.0)
	(thickness . 0.48)
	(break-align-symbol . staff-bar)
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				break-aligned-interface
				percent-repeat-interface))))))

    (DoublePercentRepeatCounter
     . (

	(stencil . ,Text_interface::print)
	(X-offset . ,(ly:make-simple-closure `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::centered_on_y_parent))
						  ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self)))))
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(font-encoding . fetaNumber)
	(self-alignment-X . 0)
	(font-size . -2) 
	(direction . 1)
	(padding . 0.2)
	(staff-padding . 0.25)
	(side-axis . ,Y)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				self-alignment-interface
				percent-repeat-interface
				font-interface
				text-interface))))))
    (DynamicLineSpanner
     . (
	(axes . (1))
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(staff-padding . 0.1)
	(padding . 0.6)
	(avoid-slur . outside)
	(slur-padding . 0.3)
	(minimum-space . 1.2)
	(direction . -1)
	(side-axis . ,Y)

	(Y-extent . ,Axis_group_interface::height)
	(X-extent . ,Axis_group_interface::width)


	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				dynamic-interface
				dynamic-line-spanner-interface
				side-position-interface))))))

    (DynamicText
     . (

	;; todo.

	(stencil . ,Text_interface::print)
	(direction . ,Script_interface::calc_direction)

	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(self-alignment-X . 0)
	(Y-offset . ,Self_alignment_interface::y_aligned_on_self)
	(self-alignment-Y . 0)
	(font-series . bold)
	(font-encoding . fetaDynamic)
	(font-shape . italic)
	(no-spacing-rods . #t)
	(script-priority . 100)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				text-interface
				self-alignment-interface
				dynamic-interface
				script-interface))))))

    (DynamicTextSpanner
     . (

	(stencil . ,Dynamic_text_spanner::print)

	;; rather ugh with NCSB
	;; (font-series . bold)
	(font-shape . italic)
	(style . dashed-line)

	;; need to blend with dynamic texts.
	(font-size . 1)
	(bound-padding . 0.75)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				text-interface
				dynamic-interface
				dynamic-text-spanner-interface
				spanner-interface))))))

    (Fingering
     . (

	;; sync with TextScript (?)

	(padding . 0.5)
	(avoid-slur . around)
	(slur-padding . 0.2)
	(staff-padding . 0.5)
	(self-alignment-X . 0)
	(self-alignment-Y . 0)
	(script-priority . 100)

	(stencil . ,Text_interface::print)
	(direction . ,Script_interface::calc_direction)

	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((class . Item)
		 (interfaces . (finger-interface
				font-interface
				text-script-interface
				text-interface
				side-position-interface
				self-alignment-interface
				item-interface))))))
    (Glissando
     . (
	(style . line)
	(gap . 0.5)
	(zigzag-width . 0.75)
	(breakable . #t)
	(X-extent . #f)
	(Y-extent . #f)

	(stencil . ,Line_spanner::print)
	(after-line-breaking . ,Line_spanner::after_line_breaking)
	

	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				line-spanner-interface))))))

    (GridPoint
     . (
	(X-extent . (0 . 0))
	(Y-extent . (0 . 0))
	(meta . ((class . Item)
		 (interfaces . (grid-point-interface))))))

    (GridLine
     . (

	(X-extent  . ,Grid_line_interface::width)
	(stencil . ,Grid_line_interface::print)

	(self-alignment-X . ,CENTER)
	(X-offset . ,(ly:make-simple-closure
		      `(,+  ,(ly:make-simple-closure (list Self_alignment_interface::centered_on_x_parent))
			    ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self)))))
	(layer . 0)
	(meta . ((class . Item)
		 (interfaces . (self-alignment-interface
				grid-line-interface))))))

    (Hairpin
     . (


	(stencil . ,Hairpin::print)
	(springs-and-rods . ,Spanner::set_spacing_rods)
	(after-line-breaking . ,Hairpin::after_line_breaking)

	(thickness . 1.0)
	(height . 0.6666)
	(minimum-length . 2.0)
	(bound-padding . 1.0)
	(self-alignment-Y . 0)
	(Y-offset . ,Self_alignment_interface::y_aligned_on_self)
	(meta . ((class . Spanner)
		 (interfaces . (hairpin-interface
				line-interface
				self-alignment-interface
				dynamic-interface
				spanner-interface))))))

    (HorizontalBracket
     . (
	(thickness . 1.0)
	(stencil . ,Horizontal_bracket::print)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(padding . 0.2)
	(staff-padding . 0.2)
	(direction . -1)
	(side-axis . ,Y)
	(bracket-flare . (0.5 . 0.5))
	(meta . ((class . Spanner)
		 (interfaces . (horizontal-bracket-interface
				side-position-interface
				line-interface
				spanner-interface))))))
    (InstrumentName
     . (
	(breakable . #t)
	(Y-offset . ,(ly:make-simple-closure `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::y_aligned_on_self))
						  ,(ly:make-simple-closure (list Side_position_interface::y_aligned_on_support_refpoints)))))
	
	;; This direction is for aligned_on_support_refpoints
	;; (?) --hwn
	(direction . 0)
	(space-alist . (
			(left-edge . (extra-space . 1.0))))

	(self-alignment-Y . 0)
	(stencil . ,Text_interface::print)
	(break-align-symbol . instrument-name)
	(break-visibility . ,begin-of-line-visible)
	(baseline-skip . 2)
	(side-axis . ,X)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				text-interface
				break-aligned-interface))))))

    (KeyCancellation
     . (

	(stencil . ,Key_signature_interface::print)

	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar . (extra-space . 0.6))
			(key-signature . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(Y-offset . ,Staff_symbol_referencer::callback)
	(break-align-symbol . key-cancellation)
	(break-visibility . ,begin-of-line-invisible)
	(breakable . #t)

	(meta . ((class . Item)
		 (interfaces . (key-signature-interface
				font-interface
				break-aligned-interface))))))
    (KeySignature
     . (

	(stencil . ,Key_signature_interface::print)

	(space-alist . (
			(time-signature . (extra-space . 1.15))
			(staff-bar . (extra-space . 1.1))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(Y-offset . ,Staff_symbol_referencer::callback)
	(break-align-symbol . key-signature)
	(break-visibility . ,begin-of-line-visible)
	(breakable . #t)

	(meta . ((class . Item)
		 (interfaces . (key-signature-interface
				font-interface
				break-aligned-interface))))))
    (LaissezVibrerTie
     . (

	(stencil  . ,Tie::print)
	(control-points . ,Laissez_vibrer_tie::calc_control_points)
	(direction . ,Laissez_vibrer_tie::calc_direction)
	
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)))
	(thickness . 1.0)
	(meta . ((class . Item)
		 (interfaces . (laissez-vibrer-tie-interface))
		 ))
	))

    (LaissezVibrerTieColumn
     . (
	(X-extent . #f)
	(Y-extent . #f)

	(positioning-done . ,Laissez_vibrer_tie_column::calc_positioning_done)
	
	(meta . ((class . Item)
		 (interfaces . (laissez-vibrer-tie-column-interface))
		 ))
	))

    (LedgerLineSpanner
     . (

	(springs-and-rods . ,Ledger_line_spanner::set_spacing_rods)

	(stencil . ,Ledger_line_spanner::print)

	(X-extent . #f)
	(Y-extent . #f)
	(minimum-length-fraction . 0.25)
	(length-fraction . 0.25)
	(layer . 0)
	(meta . ((class . Spanner)
		 (interfaces . (ledger-line-interface))))))

    (LeftEdge
     . (
	(break-align-symbol . left-edge)
	(X-extent . (0 . 0))
	(breakable . #t)
	(break-visibility . ,center-invisible)
	(space-alist . (
			(custos . (extra-space . 0.0))
			(ambitus . (extra-space . 2.0))
			(time-signature . (extra-space . 1.0))
			(staff-bar . (extra-space . 0.0))
			(breathing-sign . (minimum-space . 0.0))
			(clef . (extra-space . 0.8))
			(first-note . (fixed-space . 2.0))
			(right-edge . (extra-space . 0.0))
			(key-signature . (extra-space . 0.0))
			(key-cancellation . (extra-space . 0.0))
			))
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface))))))

    (LigatureBracket
     . (
	;; ugh.  A ligature bracket is totally different from
	;; a tuplet bracket.

	(padding . 2.0)
	(thickness . 1.6)
	(edge-height . (0.7 . 0.7))
	(shorten-pair . (-0.2 . -0.2))
	(direction . 1)
	(positions . ,Tuplet_bracket::calc_positions)
	(stencil . ,Tuplet_bracket::print)
	
	(meta . ((class . Spanner)
		 (interfaces . (tuplet-bracket-interface
				line-interface))))))

    (LyricHyphen
     . (
	(thickness . 1.3)
	(height . 0.42)
	(dash-period . 10.0)
	(length . 0.66)
	(minimum-length . 0.3)
	(minimum-distance . 0.1)
	(padding . 0.07)
	(springs-and-rods . ,Hyphen_spanner::set_spacing_rods)
	
	(stencil . ,Hyphen_spanner::print)

	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (lyric-interface
				lyric-hyphen-interface
				spanner-interface))))))

    (LyricExtender
     . (

	(stencil . ,Lyric_extender::print)

	(thickness . 0.8) ; linethickness
	(minimum-length . 1.5)
	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (lyric-interface
				lyric-extender-interface))))))

    (LyricSpace
     . ((minimum-distance . 0.3)
	(springs-and-rods . ,Hyphen_spanner::set_spacing_rods)
	(padding . 0.0)
	(Y-extent . #f)
	(X-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (spanner-interface
				lyric-hyphen-interface spacing-interface))
		 ))
	))
    (LyricText
     . (

	(stencil . ,Text_interface::print)

	(X-offset . ,Self_alignment_interface::aligned_on_x_parent)
	(self-alignment-X . 0)
	(word-space . 0.6)
	(font-series . bold-narrow)
	(font-size . 1.0)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				lyric-syllable-interface
				self-alignment-interface
				text-interface
				font-interface))))))

    (MensuralLigature
     . (
	(thickness . 1.4)
	(flexa-width . 2.0)
	(stencil . ,Mensural_ligature::print)

	(meta . ((class . Spanner)
		 (interfaces . (mensural-ligature-interface
				font-interface))))))

    (MetronomeMark
     . (

	(stencil . ,Text_interface::print)

	(Y-offset . ,Side_position_interface::y_aligned_side)
	(direction . 1)
	(padding . 0.8)
	(side-axis . ,Y)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				side-position-interface
				font-interface
				metronome-mark-interface))))))

    (MeasureGrouping
     . (
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)

	(stencil . ,Measure_grouping::print)

	(padding . 2)
	(direction . 1)
	(thickness . 1)
	(height . 2.0)
	(staff-padding . 3)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				measure-grouping-interface))))))

    (MultiMeasureRest
     . (
	(stencil . ,Multi_measure_rest::print)
	(springs-and-rods . ,Multi_measure_rest::set_spacing_rods)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(staff-position . 0)
	(expand-limit . 10)
	(thick-thickness . 6.6)
	(hair-thickness . 2.0)
	(padding . 1)
	(meta . ((class . Spanner)
		 (interfaces . (multi-measure-rest-interface
				multi-measure-interface
				rest-interface
				font-interface
				staff-symbol-referencer-interface))))))


    (MultiMeasureRestNumber
     . (
	(springs-and-rods . ,Multi_measure_rest::set_spacing_rods)
	(stencil . ,Text_interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self))
			   ,(ly:make-simple-closure (list Self_alignment_interface::x_centered_on_y_parent)))))
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)

	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.4)
	(staff-padding . 0.4)
	(font-encoding . fetaNumber)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				multi-measure-interface
				self-alignment-interface
				font-interface
				text-interface))))
	      ))

    (MultiMeasureRestText
     . (
	(stencil . ,Text_interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::x_centered_on_y_parent))
			   ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self)))))
	
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.2)
	(staff-padding . 0.25)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				multi-measure-interface
				self-alignment-interface
				font-interface
				text-interface))))))

    (NoteCollision
     . (
	(axes . (0 1))

	(X-extent . ,Axis_group_interface::width)
	(Y-extent . ,Axis_group_interface::height)
	(positioning-done . ,Note_collision_interface::calc_positioning_done)
	
	(meta . ((class . Item)
		 (interfaces . (note-collision-interface
				axis-group-interface))))))

    (NoteColumn
     . (
	(axes . (0 1))
	(X-extent . ,Axis_group_interface::width)
	(Y-extent . ,Axis_group_interface::height)

	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				note-column-interface))))))

    (NoteHead
     . (

	(stencil . ,Note_head::print)
	(stem-attachment . ,Note_head::calc_stem_attachment)
	(glyph-name . ,note-head::calc-glyph-name) 
	(Y-offset . ,Staff_symbol_referencer::callback)
	(X-offset . ,Note_head::stem_x_shift)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				rhythmic-head-interface
				font-interface
				note-head-interface
				ledgered-interface
				staff-symbol-referencer-interface))))))

    (NoteSpacing
     . (
	(stem-spacing-correction . 0.5)
	(same-direction-correction . 0.25)
	;; Changed this from 0.75.
	;; If you ever change this back, please document! --hwn
	(knee-spacing-correction . 1.0)

	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				note-spacing-interface))))))

    (NoteName
     . (

	(stencil . ,Text_interface::print)

	(meta . ((class . Item)
		 (interfaces . (note-name-interface
				text-interface
				font-interface))))))

    (OctavateEight
     . (
	(self-alignment-X . 0)
	(break-visibility . ,begin-of-line-visible)
	(X-offset . ,(ly:make-simple-closure
		      `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self))
			   ,(ly:make-simple-closure (list Self_alignment_interface::centered_on_x_parent)))))
	
	(Y-offset . ,Side_position_interface::y_aligned_side)

	(stencil . ,Text_interface::print)


	;; no Y dimensions, because of lyrics under tenor clef.
	(Y-extent . (0 . 0))
	(font-shape . italic)
	(padding . 0.6)
	(staff-padding . 0.2)
	(font-size . -4)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				self-alignment-interface
				side-position-interface
				font-interface))))))

    (OttavaBracket
     . (
	(Y-offset . ,Side_position_interface::y_aligned_side)

	(stencil . ,Ottava_bracket::print)

	(font-shape . italic)
	(shorten-pair . (0.0 . -0.6))
	(staff-padding . 1.0)
	(padding . 0.5)
	(minimum-length . 1.0)
	(dash-fraction . 0.3)
	(edge-height . (0 . 1.2))
	(direction . 1)
	(meta . ((class . Spanner)
		 (interfaces . (ottava-bracket-interface
				horizontal-bracket-interface
				line-interface
				side-position-interface
				font-interface
				text-interface))))))

    (PaperColumn
     . (
	(axes . (0))

	(before-line-breaking . ,Paper_column::before_line_breaking)
	;; (stencil . ,Paper_column::print)
	(X-extent . ,Axis_group_interface::width)
	

	;; debugging
	;;		         (font-size . -6) (font-name . "sans") (Y-extent . #f)
	(meta . ((class . Paper_column)
		 (interfaces . (paper-column-interface
				axis-group-interface
				spaceable-grob-interface))))))

    (PhrasingSlur
     . ((details . ,default-slur-details)

	(control-points . ,Slur::calc_control_points)
	(direction . ,Slur::calc_direction)
	(springs-and-rods . ,Spanner::set_spacing_rods)
	(Y-extent . ,Slur::height)
	(stencil . ,Slur::print)		      
	

	(thickness . 1.1)
	(minimum-length . 1.5)
	(height-limit . 2.0)
	(ratio . 0.333)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    (NonMusicalPaperColumn
     . (
	(axes . (0))

	(before-line-breaking . ,Paper_column::before_line_breaking)
	(X-extent . ,Axis_group_interface::width)
	;;		      (stencil . ,Paper_column::print)
	
	(breakable . #t)

	;; debugging stuff: print column number.
	;;		 (font-size . -6) (font-name . "sans")	(Y-extent . #f)

	(meta . ((class . Paper_column)
		 (interfaces . (paper-column-interface
				axis-group-interface
				spaceable-grob-interface))))))

    (PercentRepeat
     . (

	(springs-and-rods . ,Multi_measure_rest::set_spacing_rods)
	(stencil . ,Multi_measure_rest::percent)

	(slope . 1.0)
	(thickness . 0.48)
	(font-encoding . fetaMusic)
	(meta . ((class . Spanner)
		 (interfaces . (multi-measure-rest-interface
				font-interface
				percent-repeat-interface))))))
    (PercentRepeatCounter
     . (
	(stencil . ,Text_interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+ ,(ly:make-simple-closure (list Self_alignment_interface::x_centered_on_y_parent))
			   ,(ly:make-simple-closure (list Self_alignment_interface::x_aligned_on_self)))))
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.2)
	(staff-padding . 0.25)
	(font-size . -2) 
	(font-encoding . fetaNumber)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				self-alignment-interface
				percent-repeat-interface
				font-interface
				text-interface))))))

    ;; an example of a text spanner
    (PianoPedalBracket
     . (

	(stencil . ,Piano_pedal_bracket::print)

	(style . line)
	(bound-padding . 1.0)
	(direction . -1)
	(bracket-flare . (0.5 . 0.5))
	(edge-height . (1.0 . 1.0))
	(shorten-pair . (0.0 . 0.0))
	(thickness .  1.0)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				piano-pedal-interface
				piano-pedal-bracket-interface))))))

    (RehearsalMark
     . (
	(stencil . ,Text_interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+ ,(ly:make-simple-closure
			     `(,Self_alignment_interface::x_aligned_on_self))
			   ,(ly:make-simple-closure
			     `(,Self_alignment_interface::centered_on_x_parent)))
		      ))
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(self-alignment-X . 0)
	(direction . 1)
	(breakable . #t)
	(font-size . 2)
	(baseline-skip . 2)
	(break-visibility . ,end-of-line-invisible)
	(padding . 0.8)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				side-position-interface
				font-interface
				mark-interface
				self-alignment-interface))))))


    (RepeatSlash
     . (

	(stencil . ,Percent_repeat_item_interface::beat_slash)

	(thickness . 0.48)
	(slope . 1.7)
	(meta . ((class . Item)
		 (interfaces . (percent-repeat-interface))))))
    (Rest
     . (
	(stencil . ,Rest::print)
	(X-extent . ,Rest::width)
	(Y-extent . ,Rest::height)
	(Y-offset . ,Rest::y_offset_callback)
	(minimum-distance . 0.25)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				rhythmic-head-interface
				rhythmic-grob-interface
				staff-symbol-referencer-interface
				rest-interface))))))

    (RestCollision
     . (
	(minimum-distance . 0.75)
	(positioning-done . ,Rest_collision::calc_positioning_done)
					
	(meta . ((class . Item)
		 (interfaces . (rest-collision-interface))))))

    (Script
     . (
	;; don't set direction here: it breaks staccato.

	;; This value is sensitive: if too large, staccato dots will move a
	;; space a away.
	(padding . 0.20)
	(staff-padding . 0.25)
	;; (script-priority . 0) priorities for scripts, see script.scm
	(X-offset . , Self_alignment_interface::centered_on_x_parent)
	

	(stencil . ,Script_interface::print)
	(direction . ,Script_interface::calc_direction)

	(font-encoding . fetaMusic)
	(meta . ((class . Item)
		 (interfaces . (script-interface
				side-position-interface
				font-interface))))))

    (ScriptColumn
     . (

	(before-line-breaking . ,Script_column::before_line_breaking)

	(meta . ((class . Item)
		 (interfaces . (script-column-interface))))))

    (SeparationItem
     . (
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				separation-item-interface))))))

    (SeparatingGroupSpanner
     . (

	(springs-and-rods . ,Separating_group_spanner::set_spacing_rods)

	(meta . ((class . Spanner)
		 (interfaces . (only-prebreak-interface
				spacing-interface
				separation-spanner-interface))))))

    (Slur
     . ((details . ,default-slur-details)

	(control-points . ,Slur::calc_control_points)
	(direction . ,Slur::calc_direction)
	(springs-and-rods . ,Spanner::set_spacing_rods)
	(Y-extent . ,Slur::height)
	(stencil . ,Slur::print)
	
	(thickness . 1.0)
	(minimum-length . 1.5)
					; Slur::height)
	(height-limit . 2.0)
	(ratio . 0.25)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    (SpacingSpanner
     . (

	(springs-and-rods . ,Spacing_spanner::set_springs)
	(average-spacing-wishes . #t)
	(grace-space-factor . 0.6)
	(shortest-duration-space . 2.0)
	(spacing-increment . 1.2)
	(base-shortest-duration . ,(ly:make-moment 3 16))
	(meta . ((class . Spanner)
		 (interfaces . (spacing-interface
				spacing-spanner-interface))))))

    (SpanBar
     . (
	(break-align-symbol . staff-bar)
	(Y-extent . ())
	(layer . 0)
	(breakable . #t)

	(stencil . ,Span_bar::print)
	(bar-size . ,Span_bar::calc_bar_size)
	(X-extent . ,Span_bar::width)
	(glyph-name . ,Span_bar::calc_glyph_name)
	(before-line-breaking . ,Span_bar::before_line_breaking)

	;; ugh duplication!

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.6)
	(thick-thickness . 6.0)
	(meta . ((class . Item)
		 (interfaces . (span-bar-interface
				font-interface
				bar-line-interface))))))

    (StanzaNumber
     . (
	(stencil . ,Text_interface::print)

	(font-series . bold)
	(padding . 1.0)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(side-axis . ,X)
	(direction . ,LEFT)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				stanza-number-interface
				text-interface
				font-interface))))))

    (StringNumber
     . (

	(stencil . ,print-circled-text-callback)

	(padding . 0.5)
	(staff-padding . 0.5)
	(self-alignment-X . 0)
	(self-alignment-Y . 0)
	(script-priority . 100)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((class . Item)
		 (interfaces . (string-number-interface
				font-interface
				text-script-interface
				text-interface
				side-position-interface
				self-alignment-interface
				item-interface))))))

    (StaffSpacing
     . (
	(breakable . #t)
	(stem-spacing-correction . 0.4)
	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				staff-spacing-interface))))))

    (SostenutoPedal
     . (

	(stencil . ,Text_interface::print)

	(direction . 1)
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(no-spacing-rods . #t)
	(padding . 0.0) ;; padding relative to SostenutoPedalLineSpanner
	(font-shape . italic)
	(self-alignment-X . 0)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				self-alignment-interface
				font-interface))))))

    (SostenutoPedalLineSpanner
     . (
	(axes . (1))

	(X-extent . ,Axis_group_interface::height)

	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)

	(padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))

    (StaffSymbol
     . (

	(stencil . ,Staff_symbol::print)

	(line-count . 5)
	(ledger-line-thickness . (1.0 . 0.1))
	(layer . 0)
	(meta . ((class . Spanner)
		 (interfaces . (staff-symbol-interface))))))

    (Stem
     . (
	(direction . ,Stem::calc_direction)
	(stem-end-position . ,Stem::calc_stem_end_position)
	(stem-info . ,Stem::calc_stem_info)
	(positioning-done . ,Stem::calc_positioning_done)
	(stencil . ,Stem::print)
	(X-extent . ,Stem::width)
	(Y-extent . ,Stem::height)
	(length . ,Stem::calc_length)
	
	(thickness . 1.3)

	(details
	 . (
	    ;; 3.5 (or 3 measured from note head) is standard length
	    ;; 32nd, 64th flagged stems should be longer
	    (lengths . (3.5 3.5 3.5 4.5 5.0))

	    ;; FIXME.  3.5 yields too long beams (according to Ross and
	    ;; looking at Baerenreiter examples) for a number of common
	    ;; boundary cases.  Subtracting half a beam thickness fixes
	    ;; this, but the bug may well be somewhere else.

	    ;; FIXME this should come from 'lengths
	    (beamed-lengths . (3.26 3.5 3.6))

	    ;; The 'normal' minima
	    (beamed-minimum-free-lengths . (1.83 1.5 1.25))
					;(beamed-minimum-free-lengths . (2.0 1.83 1.25))

	    ;; The 'extreme case' minima
	    (beamed-extreme-minimum-free-lengths . (2.0 1.25))

	    ;; Stems in unnatural (forced) direction should be shortened by
	    ;; one staff space, according to [Roush & Gourlay].
	    ;; Flagged stems we shorten only half a staff space.
	    (stem-shorten . (1.0 0.5))

	    ))


	;; default stem direction for note on middle line
	(neutral-direction . -1)

	;; We use the normal minima as minimum for the ideal lengths,
	;; and the extreme minima as abolute minimum length.

	(X-offset . ,Stem::offset_callback)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (stem-interface
				font-interface))))))

    (StemTremolo
     . (
	(Y-extent . ,Stem_tremolo::height)
	(stencil . ,Stem_tremolo::print)
	(X-extent . #f)
	(beam-width . 1.6) ; staff-space
	(beam-thickness . 0.48) ; staff-space
	(meta . ((class . Item)
		 (interfaces . (stem-tremolo-interface))))))

    (SustainPedal
     . (
	(no-spacing-rods . #t)
	(stencil . ,Sustain_pedal::print)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.0)  ;; padding relative to SustainPedalLineSpanner
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(meta . ((class . Item)
		 (interfaces . (piano-pedal-interface
				text-spanner-interface
				text-interface
				self-alignment-interface
				font-interface))))))

    (SustainPedalLineSpanner
     . (
	(axes . (1))
	(X-extent . ,Axis_group_interface::height)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))

    (System
     . (
	(axes . (0 1))
	(X-extent . ,Axis_group_interface::width)
	(Y-extent . ,Axis_group_interface::height)
	(meta . ((class . System)
		 (interfaces . (system-interface
				axis-group-interface))))))

    (SystemStartBrace
     . (
	(style . brace)
	(padding . 0.3)
	(stencil . ,System_start_delimiter::print)
	(collapse-height . 5.0)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . ,LEFT)
	(font-encoding . fetaBraces)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (system-start-delimiter-interface
				side-position-interface
				font-interface))))))

    (SystemStartSquare
     . (
	(Y-extent . #f)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . ,LEFT)
	(stencil . ,System_start_delimiter::print)
	(style . line-bracket)
	(thickness . 1.0)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				side-position-interface
				system-start-delimiter-interface))))))
    (SystemStartBracket
     . (
	(Y-extent . #f)
	(padding . 0.8)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . ,LEFT)
	(stencil . ,System_start_delimiter::print)
	(style . bracket)
	(collapse-height . 5.0)
	(thickness . 0.45)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				side-position-interface
				system-start-delimiter-interface))))))

    (SystemStartBar
     . (
	(Y-extent . #f)
	(padding . 0.0)
	(X-offset . ,Side_position_interface::x_aligned_side)
	(direction . ,LEFT)
	(style . bar-line)
	(thickness . 1.6)
	(stencil . ,System_start_delimiter::print)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				system-start-delimiter-interface))))))


    (TabNoteHead
     . (

	(stencil . ,Text_interface::print)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(font-size . -2)
	(stem-attachment . (1.0 . 1.35))
	(font-series . bold)
	(meta . ((class . Item)
		 (interfaces
		  . (rhythmic-head-interface
		     font-interface rhythmic-grob-interface
		     note-head-interface
		     staff-symbol-referencer-interface
		     text-interface))))))

    (TextScript
     . (
	(no-spacing-rods . #t)
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(direction . -1)

	;; sync with Fingering ?
	(padding . 0.5)
	(staff-padding . 0.5)

	(stencil . ,Text_interface::print)
	(direction . ,Script_interface::calc_direction)

	(avoid-slur . around)
	(slur-padding . 0.5)
	(script-priority . 200)
	;; todo: add X self alignment?
	(meta . ((class . Item)
		 (interfaces . (text-script-interface
				text-interface
				side-position-interface
				font-interface))))))

    (TextSpanner
     . (
	(stencil . ,Text_spanner::print)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(font-shape . italic)
	(style . dashed-line)
	(staff-padding . 0.8)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(side-axis . ,Y)
	(direction . 1)
	(meta . ((class . Spanner)
		 (interfaces . (text-spanner-interface
				side-position-interface
				font-interface))))))

    (Tie
     . (
	(control-points . ,Tie::calc_control_points)
	(avoid-slur . inside)
	(direction . ,Tie::calc_direction)
	(stencil . ,Tie::print)
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)
		    (between-length-limit . 1.0)))
	(thickness . 1.0)
	(meta . ((class . Spanner)
		 (interfaces . (tie-interface))))
	))

    (TieColumn
     . (
	(positioning-done . ,Tie_column::calc_positioning_done)
	(before-line-breaking . ,Tie_column::before_line_breaking)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (tie-column-interface))))))

    (TimeSignature
     . (
	(stencil . ,Time_signature::print)
	(break-align-symbol . time-signature)
	(break-visibility . ,all-visible)
	(space-alist . (
			(first-note . (fixed-space . 2.0))
			(right-edge . (extra-space . 0.5))
			(staff-bar . (minimum-space . 2.0))))
	(breakable . #t)
	(style . C)
	(meta . ((class . Item)
		 (interfaces . (time-signature-interface
				break-aligned-interface
				font-interface))))))

    (TrillSpanner
     . (
	(stencil . ,Dynamic_text_spanner::print)
	(edge-text . ,(cons (make-musicglyph-markup "scripts.trill")
			    ""))
	(style . trill)
	(staff-padding . 1.0)
	(padding . 0.5)
	(direction . 1)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)
	(meta . ((class . Spanner)
		 (interfaces . (text-spanner-interface
				side-position-interface
				font-interface))))))

    (TrillPitchAccidental
     . ((X-offset . ,Side_position_interface::x_aligned_side)
	(padding . 0.2)
	(direction . ,LEFT)
	(font-size . -4)
	(side-axis . ,X)
	(stencil . ,Accidental_interface::print)
	(meta . ((class . Item)
		 (interfaces . (item-interface
				accidental-interface
				side-position-interface
				font-interface))))))

    (TrillPitchGroup
     . ((X-offset . ,Side_position_interface::x_aligned_side)
	(axes . (,X))
	(font-size . -4)
	(stencil . ,parenthesize-elements)
	(direction . ,RIGHT)
	(side-axis . ,X)
	(padding . 0.3)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				note-head-interface
				rhythmic-head-interface
				font-interface
				accidental-interface
				axis-group-interface))))))

    (TrillPitchHead
     . (
	(stencil . ,Note_head::print)
	(duration-log . 2)
	(Y-offset . ,Staff_symbol_referencer::callback)
	(font-size . -4)
	(meta . ((class . Item)
		 (interfaces . (item-interface
				rhythmic-head-interface
				font-interface
				pitched-trill-interface
				ledgered-interface
				staff-symbol-referencer-interface))))))

    (TupletBracket
     . (
	(padding . 1.1)
	(thickness . 1.6)
	(edge-height . (0.7 . 0.7))
	(shorten-pair . (-0.2 . -0.2))
	(staff-padding . 0.25)
	
	(direction  . ,Tuplet_bracket::calc_direction)
	(positions . ,Tuplet_bracket::calc_positions)
	(connect-to-neighbor . ,Tuplet_bracket::calc_connect_to_neighbors)
	(control-points . ,Tuplet_bracket::calc_control_points)
	(stencil . ,Tuplet_bracket::print)
	
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				tuplet-bracket-interface))))))

    (TupletNumber
     . (
	(stencil . ,Tuplet_number::print)
	(font-shape . italic)
	(font-size . -2)
	(avoid-slur . inside)
	(meta . ((class . Spanner)
		 (interfaces . (text-interface tuplet-number-interface
				font-interface))))))
    
    (UnaCordaPedal
     . (
	(stencil . ,Text_interface::print)
	(font-shape . italic)
	(no-spacing-rods . #t)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.0)  ;; padding relative to UnaCordaPedalLineSpanner
	(X-offset . ,Self_alignment_interface::x_aligned_on_self)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				self-alignment-interface
				font-interface))))))

    (UnaCordaPedalLineSpanner
     . (
	(axes . (1))
	(X-extent . ,Axis_group_interface::height)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))

    (VaticanaLigature
     . (
	(thickness . 0.6)
	(flexa-width . 2.0)
	(stencil . ,Vaticana_ligature::print)
	(meta . ((class . Spanner)
		 (interfaces . (vaticana-ligature-interface
				font-interface))))))

    (VerticalAlignment
     . (
	(axes . (1))
	(positioning-done . ,Align_interface::calc_positioning_done)
	(after-line-breaking . ,Align_interface::stretch_after_break)
	(Y-extent . ,Axis_group_interface::height)
	(X-extent . ,Axis_group_interface::width)
	(stacking-dir . -1)
	(meta . ((class . Spanner)
		 (interfaces . (align-interface
				axis-group-interface))))))
    (VerticalAxisGroup
     . (
	(axes . (1))
	(Y-offset . ,Hara_kiri_group_spanner::force_hara_kiri_callback)
	(Y-extent . ,Hara_kiri_group_spanner::y_extent)
	(X-extent . ,Axis_group_interface::width)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				hara-kiri-group-interface
				vertically-spaceable-interface))))))

    (VocalName
     . (
	(breakable . #t)
	(Y-offset . ,Side_position_interface::y_aligned_on_support_refpoints)
	(direction . 0)
	(space-alist . ((left-edge . (extra-space . 1.0))))
	(break-align-symbol . instrument-name)
	(stencil . ,Text_interface::print)
	(break-align-symbol . clef)
	(break-visibility . ,begin-of-line-visible)
	(baseline-skip . 2)
	(side-axis . ,Y)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				text-interface
				break-aligned-interface))))))

    (VoltaBracket
     . (
	(stencil . ,Volta_bracket_interface::print)
	(after-line-breaking . ,Volta_bracket_interface::after_line_breaking)
	(direction . ,UP)
	(padding . 1)
	(font-encoding . fetaNumber)
	(Y-offset . ,Side_position_interface::y_aligned_side)
	(side-axis . ,Y)
	(thickness . 1.6)  ;;  linethickness
	(edge-height . (2.0 . 2.0)) ;; staffspace;
	(minimum-space . 5)
	(font-size . -4)
	(meta . ((class . Spanner)
		 (interfaces . (volta-bracket-interface
				horizontal-bracket-interface				
				line-interface
				text-interface
				side-position-interface
				font-interface)))
	      )))


    (VoiceFollower
     . (
	(style . line)
	(gap . 0.5)
	(breakable . #t)
	(X-extent . #f)
	(Y-extent . #f)
	(stencil . ,Line_spanner::print)
	(after-line-breaking . ,Line_spanner::after_line_breaking)
	(meta . ((class . Spanner)
		 (interfaces . (line-spanner-interface
				line-interface))))
	))

))

(define (completize-grob-entry x)
  "Transplant assoc key into 'name entry of 'meta of X.  Set interfaces for Item, Spanner etc.
"
  ;;  (display (car x))
  ;;  (newline)
  (let* ((name-sym  (car x))
	 (grob-entry (cdr x))
	 (meta-entry (cdr (assoc 'meta grob-entry)))
	 (class (cdr (assoc 'class meta-entry)))
	 (ifaces-entry
	  (cdr (assoc 'interfaces meta-entry))))

    (cond
     ((eq? 'Item class)
      (set! ifaces-entry (cons 'item-interface ifaces-entry)))
     ((eq? 'Spanner class)
      (set! ifaces-entry (cons 'spanner-interface ifaces-entry)))
     ((eq? 'Paper_column class)
      (set! ifaces-entry (cons 'item-interface
			       (cons 'paper-column-interface ifaces-entry))))
     ((eq? 'System class)
      (set! ifaces-entry (cons 'system-interface
			       (cons 'spanner-interface ifaces-entry))))
     (else
      (ly:warning "Unknown class ~a" class)))
    (set! ifaces-entry (cons 'grob-interface ifaces-entry))

    (set! meta-entry (assoc-set! meta-entry 'name name-sym))
    (set! meta-entry (assoc-set! meta-entry 'interfaces
				 ifaces-entry))
    (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
    (cons name-sym grob-entry)))

(set! all-grob-descriptions (map completize-grob-entry all-grob-descriptions))

;;  (display (map pair? all-grob-descriptions))

;; make sure that \property Foo.Bar =\turnOff doesn't complain

(map (lambda (x)
       ;; (display (car x)) (newline)

       (set-object-property! (car x) 'translation-type? list?)
       (set-object-property! (car x) 'is-grob? #t))
     all-grob-descriptions)

(set! all-grob-descriptions (sort all-grob-descriptions alist<?))

