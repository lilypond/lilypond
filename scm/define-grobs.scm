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
(define-public all-grob-descriptions
  `(
    (Accidental
     . (
	(print-function . ,Accidental_interface::print)
	(inside-slur . #t)
	(cautionary-style . parentheses)
	(after-line-breaking-callback . ,Accidental_interface::after_line_breaking)
	(meta . ((interfaces . (item-interface accidental-interface
					       font-interface))))
	))

    (AccidentalPlacement
     . ((X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(left-padding . 0.2)

	;; this is quite small, but it is very ugly to have
	;; accs closer to the previous note than to the next one.
	(right-padding . 0.15)
	(meta . ((interfaces . (item-interface accidental-placement-interface))))
	))
    (Ambitus
     . (
	(axes . (0 1))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
	(space-alist . (
			(clef . (extra-space . 0.5))
			(key-signature . (extra-space . 0.0))
			(staff-bar . (extra-space . 0.0))
			(time-signature . (extra-space . 0.0))
			(first-note . (fixed-space . 0.0))
			))
	(breakable . #t)
	(break-align-symbol . ambitus)
	(break-visibility . ,begin-of-line-visible)
	(meta . ((interfaces . (axis-group-interface
				break-aligned-interface ambitus-interface item-interface))))
	))

    (AmbitusLine
     . (
       (print-function . ,Ambitus::print)
       (join-heads . #t)
       (thickness . 2)
       (X-offset-callbacks . (,Self_alignment_interface::centered_on_parent))

       (meta . ((interfaces . (ambitus-interface staff-symbol-referencer-interface item-interface font-interface))))
       ))
    (AmbitusAccidental
     . (
	(print-function . ,Accidental_interface::print)
	(font-family . music)
	(padding . 0.5)
	(X-offset-callbacks . (,Side_position_interface::aligned_side))
	(direction . -1)
	(cautionary-style . parentheses)
	(after-line-breaking-callback . ,Accidental_interface::after_line_breaking)
	(meta . ((interfaces . (item-interface
				accidental-interface break-aligned-interface
				side-position-interface font-interface))))
	))

    (AmbitusNoteHead
     . (
	(duration-log . 2)
	(style . default)
	(print-function . ,Note_head::print)
	(glyph-name-procedure . ,find-notehead-symbol)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(meta . ((interfaces . (font-interface note-head-interface
					       ambitus-interface
					       staff-symbol-referencer-interface
					       rhythmic-head-interface
					       ledgered-interface
					       item-interface ))))
	))

    (Arpeggio
     . (
	(X-extent-callback . ,Arpeggio::width_callback)
	(print-function . ,Arpeggio::print)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(X-offset-callbacks . (,Side_position_interface::aligned_side))
	(direction . -1)
	(padding . 0.5)
	(staff-position . 0.0)
	(meta . ((interfaces . (arpeggio-interface
				staff-symbol-referencer-interface side-position-interface
				item-interface font-interface))))
	))

    (BarLine
     . (
	(break-align-symbol . staff-bar)
	(glyph . "|")
	(break-glyph-function . ,default-break-barline)
	(layer . 0)
	(bar-size-procedure . ,Bar_line::get_staff_bar_size)
	(print-function . ,Bar_line::print)	
	(break-visibility . ,all-visible)
	(breakable . #t)
	(before-line-breaking-callback . ,Bar_line::before_line_breaking)
	(space-alist . (
			(time-signature . (extra-space . 0.75))
			(custos . (minimum-space . 2.0))
			(clef . (minimum-space . 1.0))
			(key-signature . (extra-space . 1.0))
			(key-cancellation . (extra-space . 1.0))
			(first-note . (fixed-space . 1.3))
			(next-note . (semi-fixed-space . 1.3))
			(right-edge . (extra-space . 0.0))
			))

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	;; TODO:
	;; kern should scale with linethickness too.
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.9)
	(thick-thickness . 6.0)
	(meta . ((interfaces . (bar-line-interface
				item-interface break-aligned-interface font-interface))))
	))


    (BarNumber
     . (
	(print-function . ,Text_interface::print)
	(breakable . #t)
	(break-visibility . ,begin-of-line-visible)
	(padding . 1.0)
	(direction . 1)
	(font-family . roman)
	(font-size . -2)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(self-alignment-X . 1)

	(meta .
	      ((interfaces . (side-position-interface
			      text-interface
			      self-alignment-interface
			      font-interface item-interface break-aligned-interface))))

	     ))

    (BassFigure
     . (
	(print-function . ,Text_interface::print)
	(Y-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(direction . 1)
	(font-family . number)

	;; We must do this, other BFs in
	;; layout16 become too small.
	(font-size . -4)
	(kern . 0.2)
	(meta . ((interfaces . (text-interface
				rhythmic-grob-interface bass-figure-interface item-interface
				self-alignment-interface font-interface))))
	))
    (Beam
     . (
	;; todo: clean this up a bit: the list is getting
	;; rather long.
	(print-function . ,Beam::print)
	(gap . 0.8)
	(positions . (#f . #f))
	(position-callbacks . (,Beam::least_squares
			       ,Beam::check_concave
			       ,Beam::slope_damping
			       ,Beam::shift_region_to_valid
			       ,Beam::quanting
			      ))

	;; TODO: should be in SLT.
	(thickness . 0.48) ; in staff-space
	(before-line-breaking-callback . ,Beam::before_line_breaking)
	(after-line-breaking-callback . ,Beam::after_line_breaking)
	(neutral-direction . -1)
	(dir-function . ,beam-dir-majority-median)
	
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
	
	(flag-width-function . ,beam-flag-width-function)
	(damping . 1)
	(auto-knee-gap . 5.5)

	;; only for debugging.
	(font-family . roman)
	
	(space-function . ,Beam::space_function)
	(meta . ((interfaces . (staff-symbol-referencer-interface beam-interface spanner-interface))))
	))

    (BreakAlignment
     . (
	(breakable . #t)
	(stacking-dir . 1)
	(break-align-orders . #(; end-of-line:
				(instrument-name
				 left-edge ambitus breathing-sign
				 clef staff-bar key-cancellation key-signature
				 time-signature custos)

				; unbroken
				(instrument-name left-edge ambitus breathing-sign
						 clef staff-bar key-cancellation key-signature
						 staff
						 time-signature custos)
				; begin of line
				(instrument-name left-edge ambitus breathing-sign
						 clef key-cancellation key-signature staff-bar
						 time-signature custos)

				))
	(axes . (0))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(meta . ((interfaces . (break-alignment-interface item-interface axis-group-interface)))))
	)

    (BreakAlignGroup
     . (
	(axes . (0))
	(X-offset-callbacks . (,Break_align_interface::alignment_callback))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(meta . ((interfaces . (break-aligned-interface item-interface axis-group-interface item-interface ))))
	))

    (BreathingSign
     . (
	(break-align-symbol . breathing-sign)
	(breakable . #t)
	(space-alist . (
			(ambitus . (extra-space . 2.0))
			(custos . (minimum-space . 1.0))
			(key-signature . (minimum-space . 1.5))
			(staff-bar . (minimum-space . 1.5))
			(clef . (minimum-space . 2.0))
			(first-note . (fixed-space . 1.0)) ;huh?
			(right-edge . (extra-space . 0.1))
			))
	(print-function . ,Text_interface::print)
	(text . ,(make-musicglyph-markup "scripts.rcomma"))
	(Y-offset-callbacks . (,Breathing_sign::offset_callback))
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((interfaces . (break-aligned-interface breathing-sign-interface text-interface font-interface item-interface ))))
	))

    (Clef
     . (
	(print-function . ,Clef::print)
	(before-line-breaking-callback . ,Clef::before_line_breaking)
	(breakable . #t)
	(font-family . music)	
	(break-align-symbol . clef)
	(break-visibility . ,begin-of-line-visible)
	(space-alist . ((ambitus . (extra-space . 2.0))
			(staff-bar . (extra-space . 0.7))
			(key-cancellation . (minimum-space . 4.0))
			(key-signature . (minimum-space . 4.0))
			(time-signature . (minimum-space . 4.2))
			(first-note . (minimum-fixed-space . 5.0))
			(next-note . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))
			))
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(meta . ((interfaces . (clef-interface
				staff-symbol-referencer-interface font-interface
				break-aligned-interface item-interface ))))
	))

    (ClusterSpannerBeacon
     . (
	(print-function . #f)
	(Y-extent-callback . ,Cluster_beacon::height)
	(meta . ((interfaces . (cluster-beacon-interface item-interface))))
	))

    (ClusterSpanner
     . (
	(print-function . ,Cluster::print)
	(spacing-procedure . ,Spanner::set_spacing_rods)		
	(minimum-length . 0.0)
	(padding . 0.25)
	(style . ramp)
	(meta . ((interfaces . (cluster-interface spanner-interface))))
	))

    (ChordName
     . (
	(print-function . ,Text_interface::print)
	(after-line-breaking-callback . ,Chord_name::after_line_breaking)
	(word-space . 0.0)
	(font-family . sans)
	(font-size . 1.5)
	(meta . ((interfaces . (font-interface
				rhythmic-grob-interface text-interface chord-name-interface
				item-interface))))
	))

    (Custos
     . (
	(break-align-symbol . custos)
	(breakable . #t)
	(print-function . ,Custos::print)
	(break-visibility . ,end-of-line-visible)
	(style . vaticana)
	(neutral-direction . -1)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(space-alist . (
			(first-note . (minimum-fixed-space . 0.0))
			(right-edge . (extra-space . 0.1))
			))
	(meta . ((interfaces
		  . (custos-interface staff-symbol-referencer-interface
				      font-interface
				      break-aligned-interface item-interface ))))
	))

    (DotColumn
     . (
	(axes . (0))
	(direction . ,RIGHT)
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(X-offset-callbacks . (,Dot_column::side_position))
	(meta . ((interfaces . (dot-column-interface axis-group-interface item-interface ))))
	))

    (Dots
     . (
	(print-function . ,Dots::print)
	(dot-count . 1)
	(meta . ((interfaces . (font-interface
				staff-symbol-referencer-interface dots-interface item-interface ))))
	))

    (DoublePercentRepeat
     . (
	(print-function . ,Percent_repeat_item_interface::double_percent)
	(breakable . #t)
	(slope . 1.0)
	(font-encoding . fetaMusic)
	(width . 2.0)
	(thickness . 0.48)
	(break-align-symbol . staff-bar)
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((interfaces . (font-interface
				break-aligned-interface
				percent-repeat-interface item-interface ))))
	))
    (DynamicText
     . (
	(Y-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(print-function . ,Text_interface::print)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(self-alignment-X . 0)
	(no-spacing-rods . #t)
	(slur-padding . 0.5)
	(script-priority . 100)
	(font-series . bold)
	(font-encoding . fetaDynamic)
	(font-shape . italic)
	(self-alignment-Y . 0)
	(meta . ((interfaces . (font-interface text-interface self-alignment-interface
					       dynamic-interface script-interface item-interface))))
	))
    (DynamicTextSpanner
     . ((print-function . ,Dynamic_text_spanner::print)

;; rather ugh with NCSB
;	(font-series . bold)
	(font-shape . italic)
	(style . dashed-line)
	(slur-padding . 0.5)

	; need to blend with dynamic texts.
	(font-size . 1)
	(bound-padding . 0.75)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(meta . ((interfaces . (font-interface
				text-interface
				dynamic-interface dynamic-text-spanner-interface
				item-interface))))
	 ))

    (DynamicLineSpanner
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(staff-padding . 0.1)
	(padding . 0.6)
	(slur-padding . 0.5)
	(minimum-space . 1.2)
	(direction . -1)
	
	(meta . ((interfaces . (dynamic-interface axis-group-interface
						  side-position-interface spanner-interface))))))

    (LeftEdge
     . (
	(break-align-symbol . left-edge)
	(X-extent . (0 . 0))
	(breakable . #t)
	(break-visibility . ,center-invisible)
	(space-alist . (
			(custos . (extra-space . 0.0))
			(ambitus . (extra-space . 2.0))
			(time-signature . (extra-space . 0.0))
			(staff-bar . (extra-space . 0.0))
			(breathing-sign . (minimum-space . 0.0))
			(clef . (extra-space . 0.8))
			(first-note . (fixed-space . 1.0))
			(right-edge . (extra-space . 0.0))
			(key-signature . (extra-space . 0.0))
			(key-cancellation . (extra-space . 0.0))
			
			))
	(meta . ((interfaces . (break-aligned-interface item-interface ))))
	))

    (Fingering
     . (
	(print-function . ,Text_interface::print)

	; sync with TextScript (?)
	(padding . 0.5)
	(slur-padding . 0.5)
	(staff-padding . 0.5)
	(self-alignment-X . 0)
	(self-alignment-Y . 0)
	(script-priority . 100)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((interfaces . (finger-interface
				font-interface text-script-interface text-interface
				side-position-interface self-alignment-interface
				item-interface))))
	))
    (StringNumber
     . (
	(print-function . ,print-circled-text-callback)
	(padding . 0.5)
	(staff-padding . 0.5)
	(self-alignment-X . 0)
	(self-alignment-Y . 0)
	(script-priority . 100)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((interfaces . (string-number-interface
				font-interface text-script-interface text-interface
				side-position-interface self-alignment-interface
				item-interface))))
	))

    (Glissando
     . (
	(style . line)
	(gap . 0.5)
	(zigzag-width . 0.75)
	(breakable . #t)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)			
	(after-line-breaking-callback . ,Line_spanner::after_line_breaking)
	(print-function . ,Line_spanner::print)
	(meta . ((interfaces . (line-interface line-spanner-interface spanner-interface))))
	))

    (GridPoint
     . (
	(X-extent . (0 . 0))
	(Y-extent . (0 . 0))
	(meta . ((interfaces . (grid-point-interface))))
	))

    (GridLine
     . (
	(print-function . ,Grid_line_interface::print)
	(X-extent-callback  . ,Grid_line_interface::width_callback)
	(self-alignment-X . ,CENTER)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self
			       ,Self_alignment_interface::centered_on_parent))
	(layer . 0)
	(meta . ((interfaces . (self-alignment-interface grid-line-interface)))
	)))
	
    (Hairpin
     . (
	(print-function . ,Hairpin::print)
	(after-line-breaking-callback . ,Hairpin::after_line_breaking)
	(thickness . 1.0)
	(height . 0.6666)
	(spacing-procedure . ,Spanner::set_spacing_rods)
	(minimum-length . 2.0)
	(bound-padding . 1.0)
	(self-alignment-Y . 0)
	(Y-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(meta . ((interfaces . (hairpin-interface
				line-interface self-alignment-interface dynamic-interface
				spanner-interface))))
	))

    (HorizontalBracket
     . (
	(thickness . 1.0)
	(print-function . ,Horizontal_bracket::print)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(padding . 0.2)
	(staff-padding . 0.2)
	(direction . -1)
	(bracket-flare . (0.5 . 0.5))
	(meta . ((interfaces . (horizontal-bracket-interface
				side-position-interface
				line-interface
				spanner-interface))))
	))
    (InstrumentName
     . (
	(breakable . #t)
	(Y-offset-callbacks . (,Self_alignment_interface::aligned_on_self
			       ,Side_position_interface::aligned_on_support_refpoints))
	;; This direction is for aligned_on_support_refpoints
	;; (?) --hwn
	(direction . 0)
	(space-alist . (
			(left-edge . (extra-space . 1.0))
			))

	(self-alignment-Y . 0)
	(print-function . ,Text_interface::print)		
	(break-align-symbol . instrument-name)
	(break-visibility . ,begin-of-line-visible)
	(baseline-skip . 2)
	(meta . ((interfaces . (font-interface
				self-alignment-interface side-position-interface text-interface
				break-aligned-interface item-interface ))))
	))

    (VocalName
     . (
	(breakable . #t)
	(Y-offset-callbacks . (,Side_position_interface::aligned_on_support_refpoints))
	(direction . 0)
	(space-alist . ((left-edge . (extra-space . 1.0))
			))
	(break-align-symbol . instrument-name)
	(print-function . ,Text_interface::print)		
	(break-align-symbol . clef)
	(break-visibility . ,begin-of-line-visible)
	(baseline-skip . 2)
	(meta . ((interfaces . (font-interface
				self-alignment-interface
				side-position-interface text-interface
				break-aligned-interface item-interface ))))
	))
    (KeyCancellation
     . (
	(print-function . ,Key_signature_interface::print)
	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar . (extra-space . 0.6))
			(key-signature . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))
			))
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(break-align-symbol . key-cancellation)
	(break-visibility . ,begin-of-line-invisible)
	(breakable . #t)
	
	(meta . ((interfaces . (key-signature-interface font-interface
							break-aligned-interface item-interface ))))
	))
    (KeySignature
     . (
	(print-function . ,Key_signature_interface::print)
	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar . (extra-space . 1.1))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))
			))
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(break-align-symbol . key-signature)
	(break-visibility . ,begin-of-line-visible)
	(breakable . #t)
	
	(meta . ((interfaces . (key-signature-interface font-interface
							break-aligned-interface item-interface ))))
	))
    (LedgerLineSpanner
     . (
	(print-function . ,Ledger_line_spanner::print)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)
	(minimum-length-fraction . 0.25)
	(length-fraction . 0.25)
	(spacing-procedure . ,Ledger_line_spanner::set_spacing_rods)
	(print-function . ,Ledger_line_spanner::print)
	(layer . 0)
	(meta . ((interfaces . (spanner-interface ledger-line-interface))))
	))

    (LigatureBracket
     . (
	(ligature-primitive-callback . ,Note_head::print)
	(direction . 1)
	(gap . 0.0)
	(padding . 2.0)
	(thickness . 1.6)
	(edge-height . (0.7 . 0.7))
	(shorten-pair . (-0.2 . -0.2))
	(before-line-breaking-callback . ,Tuplet_bracket::before_line_breaking)
	(after-line-breaking-callback . ,Tuplet_bracket::after_line_breaking)
	(print-function . ,Tuplet_bracket::print)
	(meta . ((interfaces . (tuplet-bracket-interface line-interface spanner-interface))))
	))

    (LyricHyphen
     . (
	(thickness . 1.3)
	(height . 0.42)
	(dash-period . 10.0)
	(length . 0.66)
	(minimum-length . 0.3)
	(padding . 0.07)
	(spacing-procedure . ,Hyphen_spanner::set_spacing_rods)
	(print-function . ,Hyphen_spanner::print)
	(Y-extent . (0 . 0))
	(meta . ((interfaces . (lyric-interface lyric-hyphen-interface
						spanner-interface))))
	))

    (LyricExtender
     . (
	(print-function . ,Lyric_extender::print)
	(thickness . 0.8) ; linethickness
	(minimum-length . 1.5)
	(Y-extent . (0 . 0))
	(meta . ((interfaces . (lyric-interface
				lyric-extender-interface spanner-interface))))
	))

    (LyricText
     . ((print-function . ,Text_interface::print)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_parent))
	(self-alignment-X . 0)
	(word-space . 0.6)
	(font-series . bold-narrow)
	(font-size . 1.0)
	(meta . ((interfaces . (rhythmic-grob-interface
				lyric-syllable-interface
				self-alignment-interface text-interface
				font-interface item-interface ))))
	))

    (MensuralLigature
     . (
	(thickness . 1.4)
	(flexa-width . 2.0)
	(ligature-primitive-callback . ,Mensural_ligature::brew_ligature_primitive)
	(print-function . ,Mensural_ligature::print)
	(meta . ((interfaces . (mensural-ligature-interface font-interface))))
	))

    (RehearsalMark
     . (
	(print-function . ,Text_interface::print)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(after-line-breaking-callback . ,shift-right-at-line-begin)
	(self-alignment-X . 0)
	(direction . 1)
	(breakable . #t)
	(font-size . 2)
	(baseline-skip . 2)
	(break-visibility . ,end-of-line-invisible)
	(padding . 0.8)
	(meta . ((interfaces . (text-interface
				side-position-interface font-interface mark-interface
				self-alignment-interface item-interface ))))
	))
     (MetronomeMark
     . (
	(print-function . ,Text_interface::print)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))	
	(direction . 1)
	(padding . 0.8)
	(meta . ((interfaces . (text-interface
				side-position-interface font-interface
				metronome-mark-interface item-interface))))
	))
    (MeasureGrouping
     . (
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(print-function . ,Measure_grouping::print)
	(padding . 2)
	(direction . 1)
	(thickness . 1)
	(height . 2.0)
	(staff-padding . 3)
	(meta . ((interfaces . (spanner-interface side-position-interface
						  measure-grouping-interface))))
	))
    (MultiMeasureRest
     . (
	(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
	(print-function . ,Multi_measure_rest::print)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(staff-position . 0)
	(expand-limit . 10)
	(thick-thickness . 6.6)
	(hair-thickness . 2.0)
	(padding . 1)
	(meta . ((interfaces . (multi-measure-rest-interface
				multi-measure-interface rest-interface
				font-interface staff-symbol-referencer-interface))))
	))

    (MultiMeasureRestNumber
     . (
	(print-function . ,Text_interface::print)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self
			       ,Self_alignment_interface::centered_on_other_axis_parent))
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.4)
	(staff-padding . 0.4)
	(font-encoding . fetaNumber)
	(meta . ((interfaces . (side-position-interface
				multi-measure-interface self-alignment-interface
				font-interface spanner-interface text-interface))))
	))
    (MultiMeasureRestText
     . (
	(print-function . ,Text_interface::print)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self
			       ,Self_alignment_interface::centered_on_other_axis_parent))
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.2)
	(staff-padding . 0.25)
	(meta . ((interfaces . (side-position-interface
				multi-measure-interface self-alignment-interface font-interface
				spanner-interface text-interface))))
	))

    (NoteCollision
     . (
	(axes . (0 1))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(meta . ((interfaces . (note-collision-interface axis-group-interface item-interface ))))
	))

    (NoteColumn
     . (
	(axes . (0 1))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(meta . ((interfaces . (axis-group-interface note-column-interface item-interface ))))
	))

    (NoteHead
     . (
	(style . default)
	(print-function . ,Note_head::print)
	(ligature-primitive-callback . ,Note_head::print)
	(glyph-name-procedure . ,find-notehead-symbol)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(stem-attachment-function . ,note-head-style->attachment-coordinates)
	(meta . ((interfaces . (rhythmic-grob-interface
				rhythmic-head-interface font-interface note-head-interface
				ledgered-interface
				staff-symbol-referencer-interface item-interface ))))
	))

    (NoteSpacing
     . (
	(stem-spacing-correction . 0.5)
	(same-direction-correction . 0.25)
	;; Changed this from 0.75.
	;; If you ever change this back, please document! --hwn
	(knee-spacing-correction . 1.0)
	
	(meta . ((interfaces . (spacing-interface note-spacing-interface item-interface ))))
	))

    (VoiceFollower
     . (
	(style . line)
	(gap . 0.5)
	(breakable . #t)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)			
	(print-function . ,Line_spanner::print)
	(after-line-breaking-callback . ,Line_spanner::after_line_breaking)
	(meta . ((interfaces . (line-spanner-interface line-interface spanner-interface))))
	))

    (NoteName
     . (
	(print-function . ,Text_interface::print)
	(meta . ((interfaces . (note-name-interface
				text-interface font-interface item-interface ))))
	))

    (OctavateEight
     . (
	(self-alignment-X . 0)
	(break-visibility . ,begin-of-line-visible)
	(X-offset-callbacks . (,Self_alignment_interface::centered_on_parent ,Self_alignment_interface::aligned_on_self))
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(print-function . ,Text_interface::print)

	;; no Y dimensions, because of lyrics under tenor clef.
	(Y-extent . (0 . 0))
	(font-shape . italic)
	(padding . 0.6)
	(staff-padding . 0.2)
	(font-size . -4)
	(meta . ((interfaces . (text-interface self-alignment-interface side-position-interface font-interface item-interface ))))
	))

    (PaperColumn
     . (
	(axes . (0))
	(before-line-breaking-callback . ,Paper_column::before_line_breaking)
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)

;	        (print-function . ,Paper_column::print) (font-name . "ecrm8") (Y-extent-callback . #f)
	(meta . ((interfaces . (paper-column-interface axis-group-interface spaceable-grob-interface item-interface ))))
	))

    (PhrasingSlur
     . ((slur-details . ,default-slur-details)
	(print-function . ,Slur::print)
	(thickness . 1.1)		
	(spacing-procedure . ,Spanner::set_spacing_rods)		
	(minimum-length . 1.5)
	(after-line-breaking-callback . ,Slur::after_line_breaking)
	(Y-extent-callback . ,Slur::height)
	(height-limit . 2.0)
	(ratio . 0.333)
	(meta . ((interfaces . (slur-interface spanner-interface))))
	))

    (NonMusicalPaperColumn
     . (
	(axes . (0))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(before-line-breaking-callback . ,Paper_column::before_line_breaking)
	(breakable . #t)
	;; debugging stuff: print column number.
;	(print-function . ,Paper_column::print) (font-name . "ecrm8")	(Y-extent-callback . #f)


	(meta . ((interfaces . (paper-column-interface axis-group-interface spaceable-grob-interface item-interface ))))
	))

    (PercentRepeat
     . (
	(spacing-procedure . ,Multi_measure_rest::set_spacing_rods)
	(print-function . ,Multi_measure_rest::percent)
	(slope . 1.0)
	(thickness . 0.48)
	(font-encoding . fetaMusic)
	(meta . ((interfaces . (multi-measure-rest-interface spanner-interface font-interface percent-repeat-interface))))
	))

    ;; an example of a text spanner
    (PianoPedalBracket
     . (
	(print-function . ,Piano_pedal_bracket::print)
	(style . line)
	(bound-padding . 1.0)
	(direction . -1)
	(bracket-flare . (0.5 . 0.5))
	(edge-height . (1.0 . 1.0))
	(shorten-pair . (0.0 . 0.0))
	(thickness .  1.0)
	(meta . ((interfaces . (line-interface
				piano-pedal-interface piano-pedal-bracket-interface spanner-interface))))
	))


    (RemoveEmptyVerticalGroup
     . (
	(Y-offset-callbacks . (,Hara_kiri_group_spanner::force_hara_kiri_callback))
	(Y-extent-callback . ,Hara_kiri_group_spanner::y_extent)
	(axes . (1))
	(meta . ((interfaces . (axis-group-interface
				vertically-spaceable-interface hara-kiri-group-interface
				item-interface spanner-interface))))
	))

    (RepeatSlash
     . (
	(print-function . ,Percent_repeat_item_interface::beat_slash)
	(thickness . 0.48)
	(slope . 1.7)
	(meta . ((interfaces . (percent-repeat-interface item-interface ))))
	))
    (Rest
     . (
	(after-line-breaking-callback . ,Rest::after_line_breaking)
	(X-extent-callback . ,Rest::extent_callback)
	(Y-extent-callback . ,Rest::extent_callback)		
	(print-function . ,Rest::print)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback
			       ,Rest::polyphonic_offset_callback
			       ))
	(minimum-distance . 0.25)
	(meta . ((interfaces . (font-interface
				rhythmic-head-interface
				rhythmic-grob-interface
				staff-symbol-referencer-interface
				rest-interface item-interface ))
		))))

    (RestCollision
     . (
	(minimum-distance . 0.75)
	(meta . ((interfaces . (rest-collision-interface item-interface ))))
	))

    (Script
     . (
	;; don't set direction here: it breaks staccato.
	(print-function . ,Script_interface::print)

	;; This value is sensitive: if too large, staccato dots will move a
	;; space a away.
	(padding . 0.20)
	(slur-padding . 0.5)
	(staff-padding . 0.25)
	;; (script-priority . 0) priorities for scripts, see script.scm
	(X-offset-callbacks . (,Self_alignment_interface::centered_on_parent))
	(before-line-breaking-callback . ,Script_interface::before_line_breaking)
	(font-encoding . fetaMusic)
	(meta . ((interfaces . (script-interface
				side-position-interface font-interface item-interface ))))
	))

    (ScriptColumn
     . (
	(before-line-breaking-callback . ,Script_column::before_line_breaking)
	(meta . ((interfaces . (script-column-interface item-interface ))))
	))

    (Slur
     . ((slur-details . ,default-slur-details)
	(print-function . ,Slur::print)
	(thickness . 1.0)		
	(spacing-procedure . ,Spanner::set_spacing_rods)		
	(minimum-length . 1.5)
	(after-line-breaking-callback . ,Slur::after_line_breaking)
	(Y-extent-callback . ,Slur::height)
					; Slur::height)
	(height-limit . 2.0)
	(ratio . 0.25)
	(meta . ((interfaces . (slur-interface spanner-interface))))
	))

    (SpacingSpanner
     . (
	(spacing-procedure . ,Spacing_spanner::set_springs)
	(grace-space-factor . 0.6)
	(shortest-duration-space . 2.0)
	(spacing-increment . 1.2)
	(base-shortest-duration . ,(ly:make-moment 3 16))
	(meta . ((interfaces . (spacing-interface spacing-spanner-interface spanner-interface))))
	))

    (SpanBar
     . (
	(break-align-symbol . staff-bar)
	(bar-size-procedure . ,Span_bar::get_bar_size)
	(print-function . ,Span_bar::print)
	(X-extent-callback . ,Span_bar::width_callback)
	(Y-extent-callback . ())
	(layer . 0)
	(breakable . #t)
	(before-line-breaking-callback . ,Span_bar::before_line_breaking)
	;; ugh duplication!

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.6)
	(thick-thickness . 6.0)
	(meta . ((interfaces . (span-bar-interface font-interface
						   bar-line-interface item-interface ))))
	))

    (StanzaNumber
     . ((print-function . ,Text_interface::print)		
	(font-series . bold)
	(padding . 1.0)
	(X-offset-callbacks . (,Side_position_interface::aligned_side))
	(direction . ,LEFT)
	(meta . ((interfaces . (side-position-interface
				stanza-number-interface text-interface font-interface item-interface ))))		
	))

    (StaffSpacing
     . (
	(breakable . #t)
	(stem-spacing-correction . 0.4)

	(meta . ((interfaces . (spacing-interface staff-spacing-interface item-interface ))))
	))

    (SostenutoPedal
     . (
	(print-function . ,Text_interface::print)
	(direction . 1)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(no-spacing-rods . #t)
	(padding . 0.0) ;; padding relative to SostenutoPedalLineSpanner
	(font-shape . italic)
	(self-alignment-X . 0)
	(meta . ((interfaces . (text-interface
				self-alignment-interface font-interface item-interface))))
	))

    (SostenutoPedalLineSpanner
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	

	(padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((interfaces . (piano-pedal-interface
				axis-group-interface side-position-interface spanner-interface))))
	))

    (StaffSymbol
     . (
	(print-function . ,Staff_symbol::print)
	(line-count . 5)
	(ledger-line-thickness . (1.0 . 0.1))
	(layer . 0)
	(meta . ((interfaces . (staff-symbol-interface spanner-interface))))
	))
	
    (Stem
     . (
	;; this list is rather long. Trim --hwn
	(before-line-breaking-callback . ,Stem::before_line_breaking)
	(print-function . ,Stem::print)
	(thickness . 1.3)

	;; 3.5 (or 3 measured from note head) is standard length
	;; 32nd, 64th flagged stems should be longer
	(lengths . (3.5 3.5 3.5 4.5 5.0))
	
	;; Stems in unnatural (forced) direction should be shortened by
	;; one staff space, according to [Roush & Gourlay].
	;; Flagged stems we shorten only half a staff space.
	(stem-shorten . (1.0 0.5))

	;; default stem direction for note on middle line
	(neutral-direction . -1)



	;; FIXME.  3.5 yields too long beams (according to Ross and
	;; looking at Baerenreiter examples) for a number of common
	;; boundary cases.  Subtracting half a beam thickness fixes
	;; this, but the bug may well be somewhere else.

	;; FIXME this should come from 'lengths

	(beamed-lengths . (3.26 3.5 3.6))
	
	;; We use the normal minima as minimum for the ideal lengths,
	;; and the extreme minima as abolute minimum length.
	
	;; The 'normal' minima
	(beamed-minimum-free-lengths . (1.83 1.5 1.25))
	;(beamed-minimum-free-lengths . (2.0 1.83 1.25))
	
	;; The 'extreme case' minima
	(beamed-extreme-minimum-free-lengths . (2.0 1.25))

	(X-offset-callbacks . (,Stem::offset_callback))
	(X-extent-callback . ,Stem::width_callback)	
	(Y-extent-callback . ,Stem::height)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(meta . ((interfaces . (stem-interface font-interface item-interface ))))
	))

    (StemTremolo
     . (
	(print-function . ,Stem_tremolo::print)
	(Y-extent-callback . ,Stem_tremolo::height)
	(X-extent-callback . #f)

	(beam-width . 1.6) ; staff-space
	(beam-thickness . 0.48) ; staff-space
	(meta . ((interfaces . (stem-tremolo-interface item-interface ))))
	))

    (SeparationItem
     . (
	(X-extent-callback . #f)
	(Y-extent-callback . #f)
	(meta . ((interfaces . (spacing-interface separation-item-interface item-interface ))))
	))

    (SeparatingGroupSpanner
     . (
	(spacing-procedure . ,Separating_group_spanner::set_spacing_rods)
	(meta . ((interfaces . (only-prebreak-interface spacing-interface separation-spanner-interface spanner-interface))))
	))

    (SustainPedal
     . (
	(no-spacing-rods . #t)
	(print-function . ,Sustain_pedal::print)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.0)  ;; padding relative to SustainPedalLineSpanner
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(meta . ((interfaces . (piano-pedal-interface
				text-spanner-interface text-interface self-alignment-interface
				font-interface item-interface))))
	))

    (SustainPedalLineSpanner
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((interfaces . (piano-pedal-interface
				axis-group-interface side-position-interface spanner-interface))))
	))

    (System
     . (
	(axes . (0 1))
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(meta . ((interfaces . (system-interface axis-group-interface spanner-interface))))
	))

    (SystemStartBrace
     . (
	(glyph . "brace")
	(print-function . ,System_start_delimiter::print)
	(collapse-height . 5.0)
	(font-encoding . fetaBraces)
	(Y-extent-callback . #f)
	(meta . ((interfaces . (system-start-delimiter-interface font-interface))))
	))

    (SystemStartBracket
     . (
	(Y-extent-callback . #f)
	(X-offset-callbacks . (,(lambda (g a) -0.8)))
	(print-function . ,System_start_delimiter::print)
	(glyph . "bracket")
	(collapse-height . 1)
	(thickness . 0.45)
	(meta . ((interfaces . (font-interface system-start-delimiter-interface
					       spanner-interface))))
	))

    (SystemStartBar
     . (
	(Y-extent-callback . #f)
	(print-function . ,System_start_delimiter::print)
	(glyph . "bar-line")
	(thickness . 1.6)
	(after-line-breaking-callback . ,System_start_delimiter::after_line_breaking)
	(meta . ((interfaces . (system-start-delimiter-interface spanner-interface))))
	))

    (TextScript
     . (
	(print-function . ,Text_interface::print)
	(no-spacing-rods . #t)
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(direction . -1)

	; sync with Fingering ?
	(padding . 0.5)
	(slur-padding . 0.5)
	(staff-padding . 0.5)
	(script-priority . 200)
	;; todo: add X self alignment?
	(meta . ((interfaces . (text-script-interface
				text-interface side-position-interface font-interface
				item-interface ))))
	))
    (CombineTextScript
     . (
	(print-function . ,Text_interface::print)
	(no-spacing-rods . #t)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(direction . 1)
	(padding . 0.5)
	(staff-padding . 0.5)
	(script-priority . 200)
	;; todo: add X self alignment?
	(baseline-skip . 2)
	(font-series . bold)
	(meta . ((interfaces . (text-script-interface text-interface side-position-interface font-interface item-interface ))))
	))
    (TextSpanner
     . (
	(print-function . ,Text_spanner::print)
	(font-shape . italic)
	(style . dashed-line)
	(staff-padding . 0.8)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(direction . 1)
	(meta . ((interfaces . (text-spanner-interface side-position-interface font-interface spanner-interface))))		
	))
    (TrillSpanner
     . (
	(print-function . ,Dynamic_text_spanner::print)
	(edge-text . ,(cons (make-musicglyph-markup "scripts.trill")
			    ""))
	(style . trill)
	(staff-padding . 1.0)
	(padding . 0.5)
	(direction . 1)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(meta . ((interfaces . (text-spanner-interface side-position-interface font-interface spanner-interface))))		
	))

    (OttavaBracket
     . (
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(print-function . ,Ottava_bracket::print)
	(font-shape . italic)
	(shorten-pair . (0.0 . -0.6))
	(staff-padding . 1.0)
	(padding . 0.5)
	(minimum-length . 1.0)
	(dash-fraction . 0.3)
	(edge-height . (0 . 1.2))
	(direction . 1)
	(meta . ((interfaces . (ottava-bracket-interface
				line-interface side-position-interface
				font-interface text-interface spanner-interface))))		
	))

    (TabNoteHead
     . (
	(style . default)
	(print-function . ,Text_interface::print)
	(Y-offset-callbacks . (,Staff_symbol_referencer::callback))
	(stem-attachment-function . ,tablature-stem-attachment-function)
	(font-series . bold)
	(font-size . -2)
	(meta . ((interfaces
		  . (rhythmic-head-interface
		     font-interface 
		     note-head-interface staff-symbol-referencer-interface
		     text-interface item-interface ))))
	))


    (Tie
     . (
	(print-function . ,Tie::print)
	(spacing-procedure . ,Spanner::set_spacing_rods)
	(staffline-clearance . 0.35)
	(details . ((ratio . 0.333) (height-limit . 1.0)))
	(thickness . 1.0)
	(x-gap . -0.1)
	(y-offset . 0.6)
	(minimum-length . 2.5)
	(meta . ((interfaces . (tie-interface spanner-interface))))
	))

    (TieColumn
     . (
	(after-line-breaking-callback . ,Tie_column::after_line_breaking)
	(before-line-breaking-callback . ,Tie_column::before_line_breaking)
	(X-extent-callback . #f)
	(Y-extent-callback . #f)	
	(meta . ((interfaces . (tie-column-interface spanner-interface))))
	))

    (TimeSignature
     . (
	(print-function . ,Time_signature::print)
;	(print-function . ,Text_interface::print)	
	(break-align-symbol . time-signature)
	(break-visibility . ,all-visible)
	(space-alist . (
			(first-note . (fixed-space . 2.0))
			(right-edge . (extra-space . 0.5))
			(staff-bar . (minimum-space . 2.0))
			))
	(breakable . #t)
	(style . C)
;	(text . (,time-signature-glue-markup))
	(meta . ((interfaces . (time-signature-interface break-aligned-interface font-interface item-interface ))))
	))

    (TupletBracket
     . (
	(padding . 1.1)
	(thickness . 1.6)
	(edge-height . (0.7 . 0.7))
	(shorten-pair . (-0.2 . -0.2))
	(before-line-breaking-callback . ,Tuplet_bracket::before_line_breaking)
	(after-line-breaking-callback . ,Tuplet_bracket::after_line_breaking)
	(print-function . ,Tuplet_bracket::print)
	(font-shape . italic)
;	(font-series . bold)

	(font-size . -2)
	(meta . ((interfaces . (text-interface line-interface tuplet-bracket-interface font-interface spanner-interface))))
	))

    (UnaCordaPedal
     . (
	(print-function . ,Text_interface::print)
	(font-shape . italic)
	(no-spacing-rods . #t)
	(self-alignment-X . 0)
	(direction . 1)
	(padding . 0.0)  ;; padding relative to UnaCordaPedalLineSpanner
	(X-offset-callbacks . (,Self_alignment_interface::aligned_on_self))
	(meta . ((interfaces . (text-interface self-alignment-interface font-interface item-interface ))))
	))

    (UnaCordaPedalLineSpanner
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . -1)
	(meta . ((interfaces . (piano-pedal-interface axis-group-interface side-position-interface spanner-interface))))
	))

    (VaticanaLigature
     . (
	(thickness . 0.6)
	(flexa-width . 2.0)
	(ligature-primitive-callback . ,Vaticana_ligature::brew_ligature_primitive)
	(print-function . ,Vaticana_ligature::print)
	(meta . ((interfaces . (vaticana-ligature-interface font-interface))))
	))

    (VoltaBracket
     . (
	(print-function . ,Volta_bracket_interface::print)
	(direction . 1)
	(padding . 1)
	(font-encoding . fetaNumber)
	(Y-offset-callbacks . (,Side_position_interface::aligned_side))
	(thickness . 1.6)  ;;  linethickness
	(height . 2.0) ;; staffspace;
	(minimum-space . 5)
	(font-size . -4)
	(meta . ((interfaces . (volta-bracket-interface
				line-interface text-interface
				side-position-interface font-interface spanner-interface))))
	))

    (VerticalAlignment
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	(stacking-dir . -1)
	(meta . ((interfaces . (align-interface axis-group-interface spanner-interface))))
	))

    (VerticalAxisGroup
     . (
	(axes . (1))
	(Y-extent-callback . ,Axis_group_interface::group_extent_callback)	
	(X-extent-callback . ,Axis_group_interface::group_extent_callback)
	
	(meta . ((interfaces . (axis-group-interface
				vertically-spaceable-interface spanner-interface))))
	))
   ))

(define (completize-grob-entry x)
  "transplant assoc key into 'name entry of 'meta of X
"
  (let* ((name-sym  (car x))
	 (grob-entry (cdr x))
	 (metaentry (cdr (assoc 'meta grob-entry)))
	 (ifaces-entry
	  (cdr (assoc 'interfaces metaentry)))

	)
    (set! metaentry (assoc-set! metaentry 'name name-sym))
    (set! metaentry (assoc-set! metaentry 'interfaces
				(cons 'grob-interface ifaces-entry)))
    (set! grob-entry (assoc-set! grob-entry 'meta metaentry))
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
