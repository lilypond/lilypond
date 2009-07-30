;;;; define-grobs.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; distances are given in line-thickness (thicknesses) and
;;;; staff-space (distances)

;;;; WARNING: the meta field should be the last one.
;;;; WARNING: don't use anonymous functions for initialization.

;; TODO: junk the meta field in favor of something more compact?

(define-public all-grob-descriptions
  `(
    (Accidental
     . (
	(alteration . ,accidental-interface::calc-alteration)
	(avoid-slur . inside)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(stencil . ,ly:accidental-interface::print)
	(X-extent . ,ly:accidental-interface::width)
	(Y-extent . ,ly:accidental-interface::height)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface))))))

    (AccidentalCautionary
     . (
	(alteration . ,accidental-interface::calc-alteration)
	(avoid-slur . inside)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(parenthesized . #t)
	(stencil . ,ly:accidental-interface::print)
	(Y-extent . ,ly:accidental-interface::height)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface))))))

    (AccidentalPlacement
     . (
	(direction .  ,LEFT)
	(left-padding . 0.2)
	(positioning-done . ,ly:accidental-placement::calc-positioning-done)

	;; this is quite small, but it is very ugly to have
	;; accs closer to the previous note than to the next one.
	(right-padding . 0.15)

	;; for horizontally stacked scripts.
	(script-priority .  -100)

	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Item)
		 (interfaces . (accidental-placement-interface))))))

    (AccidentalSuggestion
     . (
	(alteration . ,accidental-interface::calc-alteration)
	(direction . ,UP)
	(font-size . -2)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(outside-staff-priority . 0)
	(script-priority . 0)
	(self-alignment-X . ,CENTER)
	(side-axis . ,Y)
	(staff-padding . 0.25)
	(stencil . ,ly:accidental-interface::print)
	(X-extent . ,ly:accidental-interface::width)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-extent . ,ly:accidental-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				accidental-suggestion-interface
				font-interface
				script-interface
				self-alignment-interface
				side-position-interface))))))

    (Ambitus
     . (
	(axes . (,X ,Y))
	(break-align-symbol . ambitus)
	(break-visibility . ,begin-of-line-visible)
	(non-musical . #t)
	(space-alist . (
			(clef . (extra-space . 0.5))
			(key-signature . (extra-space . 0.0))
			(staff-bar . (extra-space . 0.0))
			(time-signature . (extra-space . 0.0))
			(first-note . (fixed-space . 0.0))))
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Item)
		 (interfaces . (ambitus-interface
				axis-group-interface
				break-aligned-interface))))))

    (AmbitusAccidental
     . (
	(direction . ,LEFT)
	(font-family . music)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(padding . 0.5)
	(side-axis . ,X)
	(stencil . ,ly:accidental-interface::print)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . ,ly:accidental-interface::height)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				break-aligned-interface
				font-interface
				side-position-interface))))))

    (AmbitusLine
     . (
	(join-heads . #t)
	(stencil . ,ly:ambitus::print)
	(thickness . 2)
	(X-offset . ,ly:self-alignment-interface::centered-on-x-parent)
	(meta . ((class . Item)
		 (interfaces . (ambitus-interface
				font-interface
				staff-symbol-referencer-interface))))))

    (AmbitusNoteHead
     . (
	(duration-log . 2)
	(glyph-name . ,note-head::calc-glyph-name)
	(stencil . ,ly:note-head::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (ambitus-interface
				font-interface
				ledgered-interface
				note-head-interface
				rhythmic-head-interface
				staff-symbol-referencer-interface))))))

    (Arpeggio
     . (
	(direction . ,LEFT)
	(padding . 0.5)
	(positions . ,ly:arpeggio::calc-positions)
	(script-priority . 0)
	(side-axis . ,X)
	(staff-position . 0.0)
	(stencil . ,ly:arpeggio::print)
	(X-extent . ,ly:arpeggio::width)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . ,ly:arpeggio::height)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (arpeggio-interface
				font-interface
				side-position-interface
				staff-symbol-referencer-interface))))))

    (BalloonTextItem
     . (
	(stencil . ,ly:balloon-interface::print)
	(text . ,(grob::calc-property-by-copy 'text))
	(X-offset . ,(grob::calc-property-by-copy 'X-offset))
	(Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
	(meta . ((class . Item)
		 (interfaces . (balloon-interface
				font-interface
				text-interface))))))

    (BarLine
     . (
	(allow-span-bar . #t)
	(bar-extent . ,ly:bar-line::calc-bar-extent)
	(bar-size .  ,ly:bar-line::calc-bar-size)
	(break-align-anchor . ,ly:bar-line::calc-anchor)
	(break-align-symbol . staff-bar)
	(break-visibility . ,bar-line::calc-break-visibility)
	(gap . 0.4)
	(glyph . "|")
	(glyph-name . ,bar-line::calc-glyph-name)

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	;; TODO:
	;; kern should scale with line-thickness too.
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.9)
	(thick-thickness . 6.0)

	(layer . 0)
	(non-musical . #t)
	(space-alist . (
			(time-signature . (extra-space . 0.75))
			(custos . (minimum-space . 2.0))
			(clef . (minimum-space . 1.0))
			(key-signature . (extra-space . 1.0))
			(key-cancellation . (extra-space . 1.0))
			(first-note . (fixed-space . 1.3))
			(next-note . (semi-fixed-space . 0.9))
			(right-edge . (extra-space . 0.0))))
	(stencil . ,ly:bar-line::print)
	(meta . ((class . Item)
		 (interfaces . (bar-line-interface
				break-aligned-interface
				font-interface))))))

    (BarNumber
     . (
	;; want the bar number before the clef at line start.
	(break-align-symbols . (left-edge staff-bar))

	(break-visibility . ,begin-of-line-visible)
	(direction . ,UP)
	(font-family . roman)
	(font-size . -2)
	(non-musical . #t)
	(outside-staff-priority . 100)
	(padding . 1.0)
	(self-alignment-X . ,RIGHT)
	(side-axis . ,Y)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:break-alignable-interface::self-align-callback))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta .
	      ((class . Item)
	       (interfaces . (break-alignable-interface
			      font-interface
			      self-alignment-interface
			      side-position-interface
			      text-interface))))))

    (BassFigure
     . (
	(stencil . ,ly:text-interface::print)
	(meta . ((class . Item)
		 (interfaces . (bass-figure-interface
				font-interface
				rhythmic-grob-interface
				text-interface))))))

    (BassFigureAlignment
     . (
	(axes . (,Y))
	(padding . 0.2)
	(positioning-done . ,ly:align-interface::align-to-minimum-distances)
	(stacking-dir . ,DOWN)
	(threshold . (2 . 1000))
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Spanner)
		 (interfaces . (align-interface
				axis-group-interface
				bass-figure-alignment-interface))))))

    (BassFigureAlignmentPositioning
     . (
	(axes . (,Y))
	(direction . ,UP)
	(padding . 0.5)
	(side-axis . ,Y)
	(staff-padding . 1.0)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				side-position-interface))))))

    (BassFigureBracket
     . (
	(edge-height . (0.2 . 0.2))
	(stencil . ,ly:enclosing-bracket::print)
	(X-extent . ,ly:enclosing-bracket::width)
	(meta . ((class . Item)
		 (interfaces . (enclosing-bracket-interface))))))

    (BassFigureContinuation
     . (
	(stencil . ,ly:figured-bass-continuation::print)
	(Y-offset . ,ly:figured-bass-continuation::center-on-figures)
	(meta . ((class . Spanner)
		 (interfaces . (figured-bass-continuation-interface))))))

    (BassFigureLine
     . (
	(adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
	(axes . (,Y))
	(vertical-skylines . ,ly:axis-group-interface::calc-skylines)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface))))))


    (Beam
     . (
	;; todo: clean this up a bit: the list is getting
	;; rather long.

	(auto-knee-gap . 5.5)

	;; We have some unreferenced problems here.
	;;
	;; If we shorten beamed stems less than normal stems (1 staff-space),
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

	(beaming . ,ly:beam::calc-beaming)
	(clip-edges . #t)
	(concaveness . ,ly:beam::calc-concaveness)
	(cross-staff . ,ly:beam::calc-cross-staff)
	(damping . 1)
	(details
         .(
           (secondary-beam-demerit . 10)
           (stem-length-demerit-factor . 5)
           (region-size . 2)
           (beam-eps . 0.001)
           (stem-length-limit-penalty . 5000)
           (damping-direction-penalty . 800)
           (hint-direction-penalty . 20)
           (musical-direction-factor . 400)
           (ideal-slope-factor . 10)
           (round-to-zero-slope . 0.02)))
	(direction . ,ly:beam::calc-direction)

	;; only for debugging.
	(font-family . roman)

	(gap . 0.8)
	(neutral-direction . ,DOWN)
	(positions .  ,(ly:make-simple-closure
			(ly:make-simple-closure
			 (list chain-grob-member-functions
			   `(,cons 0 0)
			   ly:beam::calc-least-squares-positions
			   ly:beam::slope-damping
			   ly:beam::shift-region-to-valid
			   ly:beam::quanting
			   ))))

	;; this is a hack to set stem lengths, if positions is set.
	(quantized-positions . ,ly:beam::set-stem-lengths)

	(shorten . ,ly:beam::calc-stem-shorten)
	(stencil . ,ly:beam::print)

	;; TODO: should be in SLT.
	(thickness . 0.48) ; in staff-space

	(meta . ((class . Spanner)
		 (object-callbacks . ((normal-stems . ,ly:beam::calc-normal-stems)))
		 (interfaces . (beam-interface
				font-interface
				staff-symbol-referencer-interface
				unbreakable-spanner-interface))))))

    (BendAfter
     . (
	(minimum-length . 0.5)
	(stencil . ,bend::print)
	(thickness . 2.0)
	(meta . ((class . Spanner)
		 (interfaces . (bend-after-interface
				spanner-interface))))))

    (BreakAlignGroup
     . (
	(axes . (,X))
	(break-align-anchor . ,ly:break-aligned-interface::calc-average-anchor)
	(break-visibility . ,ly:break-aligned-interface::calc-break-visibility)
	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				break-aligned-interface))))))

    (BreakAlignment
     . (
	(axes . (,X))
	(break-align-orders . ;; end of line
			    #((
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
			      (
			       left-edge
			       ambitus
			       breathing-sign
			       clef
			       staff-bar
			       key-cancellation
			       key-signature
			       time-signature
			       custos)

			      ;; begin of line
			      (
			       left-edge
			       ambitus
			       breathing-sign
			       clef
			       key-cancellation
			       key-signature
			       staff-bar
			       time-signature
			       custos)))
	(non-musical . #t)
	(positioning-done . ,ly:break-alignment-interface::calc-positioning-done)
	(stacking-dir . 1)
	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				break-alignment-interface))))))

    (BreathingSign
     . (
	(break-align-symbol . breathing-sign)
	(break-visibility . ,begin-of-line-invisible)
	(non-musical . #t)
	(space-alist . (
			(ambitus . (extra-space . 2.0))
			(custos . (minimum-space . 1.0))
			(key-signature . (minimum-space . 1.5))
			(time-signature . (minimum-space . 1.5))
			(staff-bar . (minimum-space . 1.5))
			(clef . (minimum-space . 2.0))
			(first-note . (fixed-space . 1.0)) ;huh?
			(right-edge . (extra-space . 0.1))))
	(stencil . ,ly:text-interface::print)
	(text . ,(make-musicglyph-markup "scripts.rcomma"))
	(Y-offset . ,ly:breathing-sign::offset-callback)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				breathing-sign-interface
				font-interface
				text-interface))))))

    (ChordName
     . (
	(after-line-breaking . ,ly:chord-name::after-line-breaking)
	(font-family . sans)
	(font-size . 1.5)
	(stencil . ,ly:text-interface::print)
	(word-space . 0.0)
	(meta . ((class . Item)
		 (interfaces . (chord-name-interface
				font-interface
				rhythmic-grob-interface
				text-interface))))))

    (Clef
     . (
	(avoid-slur . inside)
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-align-symbol . clef)
	(break-visibility . ,begin-of-line-visible)
	(font-family . music)
	(glyph-name . ,ly:clef::calc-glyph-name)
	(non-musical . #t)
	(space-alist . ((ambitus . (extra-space . 2.0))
			(staff-bar . (extra-space . 0.7))
			(key-cancellation . (minimum-space . 3.5))
			(key-signature . (minimum-space . 3.5))
			(time-signature . (minimum-space . 4.2))
			(first-note . (minimum-fixed-space . 5.0))
			(next-note . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))))
	(stencil . ,ly:clef::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				clef-interface
				font-interface
				staff-symbol-referencer-interface))))))

    (ClusterSpanner
     . (
	(cross-staff . ,ly:cluster::calc-cross-staff)
	(minimum-length . 0.0)
	(padding . 0.25)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:cluster::print)
	(style . ramp)
	(meta . ((class . Spanner)
		 (interfaces . (cluster-interface))))))

    (ClusterSpannerBeacon
     . (
	(Y-extent . ,ly:cluster-beacon::height)
	(meta . ((class . Item)
		 (interfaces . (cluster-beacon-interface
				rhythmic-grob-interface))))))

    (CombineTextScript
     . (
	(avoid-slur . outside)
	(baseline-skip . 2)
	(direction . ,UP)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(font-series . bold)
	(padding . 0.5)
	(script-priority . 200)
	(side-axis . ,Y)
	(staff-padding . 0.5)
	;; todo: add X self alignment?
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				side-position-interface
				text-interface
				text-script-interface))))))

    (Custos
     . (
	(break-align-symbol . custos)
	(break-visibility . ,end-of-line-visible)
	(neutral-direction . ,DOWN)
	(non-musical . #t)
	(space-alist . (
			(first-note . (minimum-fixed-space . 0.0))
			(right-edge . (extra-space . 0.1))))
	(stencil . ,ly:custos::print)
	(style . vaticana)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces  . (break-aligned-interface
				 custos-interface
				 font-interface
				 staff-symbol-referencer-interface))))))

    (DotColumn
     . (
	(axes . (,X))
	(direction . ,RIGHT)
	(positioning-done . ,ly:dot-column::calc-positioning-done)
	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				dot-column-interface))))))

    (Dots
     . (
	(dot-count . ,dots::calc-dot-count)
	(staff-position . ,dots::calc-staff-position)
	(stencil . ,ly:dots::print)
	(meta . ((class . Item)
		 (interfaces . (dots-interface
				font-interface
				staff-symbol-referencer-interface))))))

    (DoublePercentRepeat
     . (
	(break-align-symbol . staff-bar)
	(break-visibility . ,begin-of-line-invisible)
	(dot-negative-kern . 0.75)
	(font-encoding . fetaMusic)
	(non-musical . #t)
	(slash-negative-kern . 1.6)
	(slope . 1.0)
	(stencil . ,ly:percent-repeat-item-interface::double-percent)
	(thickness . 0.48)
	(width . 2.0)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				font-interface
				percent-repeat-interface
				percent-repeat-item-interface))))))

    (DoublePercentRepeatCounter
     . (
	(direction . ,UP)
	(font-encoding . fetaNumber)
	(font-size . -2)
	(padding . 0.2)
	(self-alignment-X . ,CENTER)
	(side-axis . ,Y)
	(staff-padding . 0.25)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				percent-repeat-interface
				percent-repeat-item-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))

    (DynamicLineSpanner
     . (
	(axes . (,Y))
	(cross-staff . ,ly:side-position-interface::calc-cross-staff)
	(direction . ,DOWN)
	(minimum-space . 1.2)
	(outside-staff-priority . 250)
	(padding . 0.6)
	(side-axis . ,Y)
	(slur-padding . 0.3)
	(staff-padding . 0.1)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				dynamic-interface
				dynamic-line-spanner-interface
				side-position-interface))))))

    (DynamicText
     . (

	;; todo.

	(direction . ,ly:script-interface::calc-direction)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(font-encoding . fetaDynamic)
	(font-series . bold)
	(font-shape . italic)
	(outside-staff-priority . 250)
	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:self-alignment-interface::y-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (dynamic-interface
				font-interface
				script-interface
				self-alignment-interface
				text-interface))))))

    (DynamicTextSpanner
     . (
	(bound-details . ((right . ((attach-dir .  ,LEFT)
				    (Y . 0)
				    (padding . 0.75)
				    ))
			  (right-broken . ((attach-dir .  ,RIGHT)
				    (padding . 0.0)
				    ))

			  (left . ((attach-dir .  ,LEFT)
				   (Y . 0)
				   (stencil-offset . (0 . -0.5))
				   (padding . 0.5)
				   ))
			  (left-broken . ((attach-dir .  ,RIGHT)
				   ))
			  ))
	(dash-fraction . 0.2)
	(dash-period . 3.0)

	;; rather ugh with NCSB
	;; (font-series . bold)
	(font-shape . italic)

	;; need to blend with dynamic texts.
	(font-size . 1)

	(left-bound-info . ,ly:line-spanner::calc-left-bound-info-and-text)

	;; make sure the spanner doesn't get too close to notes
	(minimum-Y-extent . (-1 . 1))

	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(stencil . ,ly:line-spanner::print)
	(style . dashed-line)
	(meta . ((class . Spanner)
		 (interfaces . (dynamic-interface
				dynamic-text-spanner-interface
				font-interface
				line-interface
				line-spanner-interface
				spanner-interface
				text-interface))))))


    (Fingering
     . (

	;; sync with TextScript (?)

	(avoid-slur . around)
	(cross-staff . ,ly:side-position-interface::calc-cross-staff)
	(direction . ,ly:script-interface::calc-direction)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(padding . 0.5)
	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(script-priority . 100)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(slur-padding . 0.2)
	(staff-padding . 0.5)
	(stencil . ,ly:text-interface::print)
	(text . ,fingering::calc-text)
	(meta . ((class . Item)
		 (interfaces . (finger-interface
				font-interface
				self-alignment-interface
				side-position-interface
				text-interface
				text-script-interface))))))

    (FretBoard
     . (
	(after-line-breaking . ,ly:chord-name::after-line-breaking)
	(fret-diagram-details . ((finger-code . below-string)))
	(stencil . ,fret-board::calc-stencil)
	(meta . ((class . Item)
		 (interfaces . (chord-name-interface
				font-interface
				fret-diagram-interface
				rhythmic-grob-interface))))))


    (Glissando
     . (
	(after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
	(bound-details . ((right . ((attach-dir .  ,CENTER)
				    (padding . 1.5)
				      ))
			  (left . ((attach-dir .  ,CENTER)
				   (padding . 1.5)
				      ))
			  ))
	(gap . 0.5)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(stencil . ,ly:line-spanner::print)
	(style . line)
	(X-extent . #f)
	(Y-extent . #f)
	(zigzag-width . 0.75)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				line-spanner-interface
				unbreakable-spanner-interface))))))

    (GraceSpacing
     . (
	(common-shortest-duration . ,grace-spacing::calc-shortest-duration)
	(shortest-duration-space . 1.6)
	(spacing-increment . 0.8)
	(meta . ((class . Spanner)
		 (interfaces . (grace-spacing-interface
				spacing-options-interface
				spanner-interface))))))

    (GridLine
     . (
	(layer . 0)
	(self-alignment-X . ,CENTER)
	(stencil . ,ly:grid-line-interface::print)
	(X-extent  . ,ly:grid-line-interface::width)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(meta . ((class . Item)
		 (interfaces . (grid-line-interface
				self-alignment-interface))))))

    (GridPoint
     . (
	(X-extent . (0 . 0))
	(Y-extent . (0 . 0))
	(meta . ((class . Item)
		 (interfaces . (grid-point-interface))))))


    (Hairpin
     . (
	(after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
	(bound-padding . 1.0)
	(circled-tip . #f)
	(grow-direction . ,hairpin::calc-grow-direction)
	(height . 0.6666)
	(minimum-length . 2.0)
	(self-alignment-Y . ,CENTER)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:hairpin::print)
	(thickness . 1.0)
	(to-barline . #t)
	(Y-extent . ,ly:hairpin::height)
	(Y-offset . ,ly:self-alignment-interface::y-aligned-on-self)
	(meta . ((class . Spanner)
		 (interfaces . (dynamic-interface
				hairpin-interface
				line-interface
				self-alignment-interface
				spanner-interface))))))

    (HarmonicParenthesesItem
     . (
	(padding . 0)
	(stencil . ,parentheses-item::print)
	(stencils . ,parentheses-item::calc-angled-bracket-stencils)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				parentheses-interface))))))

    (HorizontalBracket
     . (
	(bracket-flare . (0.5 . 0.5))
	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	(direction . ,DOWN)
	(padding . 0.2)
	(side-axis . ,Y)
	(staff-padding . 0.2)
	(stencil . ,ly:horizontal-bracket::print)
	(thickness . 1.0)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (horizontal-bracket-interface
				line-interface
				side-position-interface
				spanner-interface))))))


    (InstrumentName
     . (
	(direction . ,LEFT)
	(padding . 0.3)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(stencil . ,ly:system-start-text::print)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				system-start-text-interface))))))

    (InstrumentSwitch
     . (
	(direction . ,UP)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(outside-staff-priority . 500)
	(padding . 0.5)
	(self-alignment-X . ,LEFT)
	(side-axis . ,Y)
	(staff-padding . 0.5)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))


    (KeyCancellation
     . (
	(break-align-symbol . key-cancellation)
	(break-visibility . ,begin-of-line-invisible)
	(glyph-name-alist . ,cancellation-glyph-name-alist)
	(non-musical . #t)
	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar . (extra-space . 0.6))
			(key-signature . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(stencil . ,ly:key-signature-interface::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				font-interface
				key-cancellation-interface
				key-signature-interface
				staff-symbol-referencer-interface))))))

    (KeySignature
     . (
	(avoid-slur . inside)
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-align-symbol . key-signature)
	(break-visibility . ,begin-of-line-visible)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(non-musical . #t)
	(space-alist . (
			(time-signature . (extra-space . 1.15))
			(staff-bar . (extra-space . 1.1))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(stencil . ,ly:key-signature-interface::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				font-interface
				key-signature-interface
				staff-symbol-referencer-interface))))))


   (LaissezVibrerTie
     . (
	(control-points . ,ly:semi-tie::calc-control-points)
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)))
	(direction . ,ly:tie::calc-direction)
	(head-direction . ,LEFT)
	(stencil  . ,ly:tie::print)
	(thickness . 1.0)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-interface))))))

    (LaissezVibrerTieColumn
     . (
	(head-direction . ,LEFT)
	(positioning-done . ,ly:semi-tie-column::calc-positioning-done)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-column-interface))))))

    (LedgerLineSpanner
     . (
	(layer . 0)
	(length-fraction . 0.25)
	(minimum-length-fraction . 0.25)
	(springs-and-rods . ,ly:ledger-line-spanner::set-spacing-rods)
	(stencil . ,ly:ledger-line-spanner::print)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (ledger-line-spanner-interface))))))

    (LeftEdge
     . (
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-align-symbol . left-edge)
	(break-visibility . ,center-invisible)
	(non-musical . #t)
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
	(X-extent . (0 . 0))
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface))))))

    (LigatureBracket
     . (
	;; ugh.  A ligature bracket is totally different from
	;; a tuplet bracket.

	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	(control-points . ,ly:tuplet-bracket::calc-control-points)
	(direction . ,UP)
	(edge-height . (0.7 . 0.7))
	(padding . 2.0)
	(positions . ,ly:tuplet-bracket::calc-positions)
	(shorten-pair . (-0.2 . -0.2))
	(staff-padding . 0.25)
	(stencil . ,ly:tuplet-bracket::print)
	(thickness . 1.6)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				tuplet-bracket-interface))))))

    (LyricExtender
     . (
	(minimum-length . 1.5)
	(stencil . ,ly:lyric-extender::print)
	(thickness . 0.8) ; line-thickness
	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (lyric-extender-interface
				lyric-interface))))))

    (LyricHyphen
     . (
	(dash-period . 10.0)
	(height . 0.42)
	(length . 0.66)
	(minimum-distance . 0.1)
	(minimum-length . 0.3)
	(padding . 0.07)
	(springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
	(stencil . ,ly:lyric-hyphen::print)
	(thickness . 1.3)
	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				lyric-hyphen-interface
				lyric-interface
				spanner-interface))))))

    (LyricSpace
     . (
	(minimum-distance . 0.45)
	(padding . 0.0)
	(springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (lyric-hyphen-interface
				spanner-interface))))))

    (LyricText
     . (
	(extra-spacing-width . (0.0 . 0.0))
	(font-series . bold-narrow)
	(font-size . 1.0)
	(self-alignment-X . ,CENTER)
	(stencil . ,lyric-text::print)
	(text . ,(grob::calc-property-by-copy 'text))
	(word-space . 0.6)
	(X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				lyric-syllable-interface
				rhythmic-grob-interface
				self-alignment-interface
				text-interface))))))


    (MeasureGrouping
     . (
	(direction . ,UP)
	(height . 2.0)
	(padding . 2)
	(side-axis . ,Y)
	(staff-padding . 3)
	(stencil . ,ly:measure-grouping::print)
	(thickness . 1)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (measure-grouping-interface
				side-position-interface))))))

    (MelodyItem
     . (
	(neutral-direction . ,DOWN)
	(meta . ((class . Item)
		 (interfaces . (melody-spanner-interface))))))

    (MensuralLigature
     . (
	(flexa-width . 2.0)
	(stencil . ,ly:mensural-ligature::print)
	(thickness . 1.4)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				mensural-ligature-interface))))))

    (MetronomeMark
     . (
	(direction . ,UP)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(outside-staff-priority . 1000)
	(padding . 0.8)
	(side-axis . ,Y)
	(stencil . ,ly:text-interface::print)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				metronome-mark-interface
				side-position-interface
				text-interface))))))

    (MultiMeasureRest
     . (
	(expand-limit . 10)
	(hair-thickness . 2.0)
	(padding . 1)
	(springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
	(staff-position . 0)
	(stencil . ,ly:multi-measure-rest::print)
	(thick-thickness . 6.6)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				multi-measure-interface
				multi-measure-rest-interface
				rest-interface
				staff-symbol-referencer-interface))))))

    (MultiMeasureRestNumber
     . (
	(bound-padding  . 2.0)
	(direction . ,UP)
	(font-encoding . fetaNumber)
	(padding . 0.4)
	(self-alignment-X . ,CENTER)
	(side-axis . ,Y)
	(springs-and-rods . ,ly:multi-measure-rest::set-text-rods)
	(staff-padding . 0.4)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				multi-measure-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))

    (MultiMeasureRestText
     . (
	(direction . ,UP)
	(outside-staff-priority . 450)
	(padding . 0.2)
	(self-alignment-X . ,CENTER)
	(staff-padding . 0.25)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				multi-measure-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))


    (NonMusicalPaperColumn
     . (
	(allow-loose-spacing . #t)
	(axes . (,X))
	(before-line-breaking . ,ly:paper-column::before-line-breaking)
	(full-measure-extra-space . 1.0)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	;;		      (stencil . ,ly:paper-column::print)

	(line-break-permission . allow)
	(non-musical . #t)
	(page-break-permission . allow)

	;; debugging stuff: print column number.
	;;		 (font-size . -6) (font-name . "sans")	(Y-extent . #f)

	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Paper_column)
		 (interfaces . (axis-group-interface
				font-interface
				paper-column-interface
				separation-item-interface
				spaceable-grob-interface))))))

    (NoteCollision
     . (
	(axes . (,X ,Y))
	(positioning-done . ,ly:note-collision-interface::calc-positioning-done)
	(prefer-dotted-right . #t)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				note-collision-interface))))))

    (NoteColumn
     . (
	(axes . (,X ,Y))
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				note-column-interface
				separation-item-interface))))))

    (NoteHead
     . (
	(duration-log . ,note-head::calc-duration-log)
	(glyph-name . ,note-head::calc-glyph-name)
	(stem-attachment . ,ly:note-head::calc-stem-attachment)
	(stencil . ,ly:note-head::print)
	(X-offset . ,ly:note-head::stem-x-shift)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				gregorian-ligature-interface
				ledgered-interface
				mensural-ligature-interface
				note-head-interface
				rhythmic-grob-interface
				rhythmic-head-interface
				staff-symbol-referencer-interface
				vaticana-ligature-interface))))))

    (NoteName
     . (
	(stencil . ,ly:text-interface::print)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				note-name-interface
				text-interface))))))

    (NoteSpacing
     . (
	;; Changed this from 0.75.
	;; If you ever change this back, please document! --hwn
	(knee-spacing-correction . 1.0)
	(same-direction-correction . 0.25)
	(space-to-barline . #t)
	(stem-spacing-correction . 0.5)
	(meta . ((class . Item)
		 (interfaces . (note-spacing-interface
				spacing-interface))))))


    (OctavateEight
     . (
	(break-visibility . ,begin-of-line-visible)
	(font-shape . italic)
	(font-size . -4)
	(self-alignment-X . ,CENTER)
	(staff-padding . 0.2)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))

    (OttavaBracket
     . (
	(dash-fraction . 0.3)
	(direction . ,UP)
	(edge-height . (0 . 1.2))
	(font-shape . italic)
	(minimum-length . 1.0)
	(outside-staff-priority . 400)
	(padding . 0.5)
	(shorten-pair . (0.0 . -0.6))
	(staff-padding . 1.0)
	(stencil . ,ly:ottava-bracket::print)
	(style . dashed-line)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				horizontal-bracket-interface
				line-interface
				ottava-bracket-interface
				side-position-interface
				text-interface))))))


    (PaperColumn
     . (
	(allow-loose-spacing . #t)
	(axes . (,X))
	(before-line-breaking . ,ly:paper-column::before-line-breaking)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	;; (stencil . ,ly:paper-column::print)
	(X-extent . ,ly:axis-group-interface::width)

	;; debugging
	;;		         (font-size . -6) (font-name . "sans") (Y-extent . #f)
	(meta . ((class . Paper_column)
		 (interfaces . (axis-group-interface
				font-interface
				paper-column-interface
				separation-item-interface
				spaceable-grob-interface))))))

    (ParenthesesItem
     . (
	(font-size . -6)
	(padding . 0.2)
	(stencil . ,parentheses-item::print)
	(stencils . ,parentheses-item::calc-parenthesis-stencils)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				parentheses-interface))))))

    (PercentRepeat
     . (
	(dot-negative-kern . 0.75)
	(font-encoding . fetaMusic)
	(slope . 1.0)
	(springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
	(stencil . ,ly:multi-measure-rest::percent)
	(thickness . 0.48)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				multi-measure-rest-interface
				percent-repeat-interface))))))

    (PercentRepeatCounter
     . (
	(direction . ,UP)
	(font-encoding . fetaNumber)
	(font-size . -2)
	(padding . 0.2)
	(self-alignment-X . ,CENTER)
	(staff-padding . 0.25)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				percent-repeat-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))

    (PhrasingSlur
     . (
	(control-points . ,ly:slur::calc-control-points)
	(cross-staff . ,ly:slur::calc-cross-staff)
	(details . ,default-slur-details)
	(direction . ,ly:slur::calc-direction)
	(height-limit . 2.0)
	(minimum-length . 1.5)
	(ratio . 0.333)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:slur::print)
	(thickness . 1.1)
	(Y-extent . ,ly:slur::height)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    ;; an example of a text spanner
    (PianoPedalBracket
     . (
	(bound-padding . 1.0)
	(bracket-flare . (0.5 . 0.5))
	(direction . ,DOWN)
	(edge-height . (1.0 . 1.0))
	(shorten-pair . (0.0 . 0.0))
	(stencil . ,ly:piano-pedal-bracket::print)
	(style . line)
	(thickness .  1.0)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				piano-pedal-bracket-interface
				piano-pedal-interface))))))


    (RehearsalMark
     . (
	(baseline-skip . 2)
	(break-align-symbols . (staff-bar clef))
	(break-visibility . ,end-of-line-invisible)
	(direction . ,UP)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(font-size . 2)
	(non-musical . #t)
	(outside-staff-priority . 1500)
	(padding . 0.8)
	(self-alignment-X . ,CENTER)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:break-alignable-interface::self-align-callback))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (break-alignable-interface
				font-interface
				mark-interface
				self-alignment-interface
				side-position-interface
				text-interface))))))

    (RepeatSlash
     . (
	(slope . 1.7)
	(stencil . ,ly:percent-repeat-item-interface::beat-slash)
	(thickness . 0.48)
	(meta . ((class . Item)
		 (interfaces . (percent-repeat-interface
				percent-repeat-item-interface
				rhythmic-grob-interface))))))

    (RepeatTie
     . (
	(control-points . ,ly:semi-tie::calc-control-points)
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)))
	(direction . ,ly:tie::calc-direction)
	(head-direction . ,RIGHT)
	(stencil  . ,ly:tie::print)
	(thickness . 1.0)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-interface))))))

    (RepeatTieColumn
     . (
	(direction . ,ly:tie::calc-direction)
	(head-direction . ,ly:semi-tie-column::calc-head-direction)
	(positioning-done . ,ly:semi-tie-column::calc-positioning-done)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-column-interface))))))

    (Rest
     . (
	(cross-staff . ,ly:rest::calc-cross-staff)
	(duration-log . ,stem::calc-duration-log)
	(minimum-distance . 0.25)
	(stencil . ,ly:rest::print)
	(X-extent . ,ly:rest::width)
	(Y-extent . ,ly:rest::height)
	(Y-offset . ,ly:rest::y-offset-callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				rest-interface
				rhythmic-grob-interface
				rhythmic-head-interface
				staff-symbol-referencer-interface))))))

    (RestCollision
     . (
	(minimum-distance . 0.75)
	(positioning-done . ,ly:rest-collision::calc-positioning-done)
	(meta . ((class . Item)
		 (interfaces . (rest-collision-interface))))))


    (Script
     . (
	(cross-staff . ,ly:script-interface::calc-cross-staff)
	(direction . ,ly:script-interface::calc-direction)
	(font-encoding . fetaMusic)
	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(side-axis . ,Y)

	;; padding set in script definitions.
	(staff-padding . 0.25)

	(stencil . ,ly:script-interface::print)
	(X-offset . ,script-interface::calc-x-offset)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				script-interface
				side-position-interface))))))

    (ScriptColumn
     . (
	(before-line-breaking . ,ly:script-column::before-line-breaking)
	(meta . ((class . Item)
		 (interfaces . (script-column-interface))))))

    (ScriptRow
     . (
	(before-line-breaking . ,ly:script-column::row-before-line-breaking)
	(meta . ((class . Item)
		 (interfaces . (script-column-interface))))))

    (SeparationItem
     . (
	(avoid-slur . inside)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	(stencil . ,ly:separation-item::print)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Item)
		 (interfaces . (separation-item-interface))))))

    (Slur
     . (
	(avoid-slur . inside)
	(control-points . ,ly:slur::calc-control-points)
	(cross-staff . ,ly:slur::calc-cross-staff)
	(details . ,default-slur-details)
	(direction . ,ly:slur::calc-direction)
	(height-limit . 2.0)
	(line-thickness . 0.8)
	(minimum-length . 1.5)
	(ratio . 0.25)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:slur::print)
	(thickness . 1.2)
	(Y-extent . ,ly:slur::height)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    (SostenutoPedal
     . (
	(direction . ,RIGHT)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(font-shape . italic)
	(padding . 0.0) ;; padding relative to SostenutoPedalLineSpanner
	(self-alignment-X . ,CENTER)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				piano-pedal-script-interface
				self-alignment-interface
				text-interface))))))

    (SostenutoPedalLineSpanner
     . (
	(axes . (,Y))
	(direction . ,DOWN)
	(minimum-space . 1.0)
	(outside-staff-priority . 1000)
	(padding . 1.2)
	(side-axis . ,Y)
	(staff-padding . 1.0)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				piano-pedal-interface
				side-position-interface))))))

    (SpacingSpanner
     . (
	(average-spacing-wishes . #t)
	(base-shortest-duration . ,(ly:make-moment 3 16))
	(common-shortest-duration . ,ly:spacing-spanner::calc-common-shortest-duration)
	(shortest-duration-space . 2.0)
	(spacing-increment . 1.2)
	(springs-and-rods . ,ly:spacing-spanner::set-springs)
	(meta . ((class . Spanner)
		 (interfaces . (spacing-options-interface
				spacing-spanner-interface))))))

    (SpanBar
     . (
	(allow-span-bar . #t)
	(bar-extent . ,ly:axis-group-interface::height)
	(bar-size . ,ly:span-bar::calc-bar-size)
	(before-line-breaking . ,ly:span-bar::before-line-breaking)
	(break-align-symbol . staff-bar)
	(cross-staff . #t)
	(glyph-name . ,ly:span-bar::calc-glyph-name)

	;; ugh duplication! (these 4 properties were copied from Barline)
	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	(kern . 3.0)
	(thin-kern . 3.0)
	(hair-thickness . 1.6)
	(thick-thickness . 6.0)

	(layer . 0)
	(non-musical . #t)
	(stencil . ,ly:span-bar::print)
	(X-extent . ,ly:span-bar::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Item)
		 (interfaces . (bar-line-interface
				font-interface
				span-bar-interface))))))

    (StaffGrouper
     . (
	(between-staff-spacing . ((space . 9) (minimum-distance . 7)))
	(after-last-staff-spacing . ((space . 10.5) (minimum-distance . 8)))
	(meta . ((class . Spanner)
		 (interfaces . (staff-grouper-interface))))))

    (StaffSpacing
     . (
	(non-musical . #t)
	(stem-spacing-correction . 0.4)
	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				staff-spacing-interface))))))

    (StaffSymbol
     . (
	(layer . 0)
	(ledger-line-thickness . (1.0 . 0.1))
	(line-count . 5)
	(stencil . ,ly:staff-symbol::print)
	(Y-extent . ,ly:staff-symbol::height)
	(meta . ((class . Spanner)
		 (interfaces . (staff-symbol-interface))))))

    (StanzaNumber
     . (
	(direction . ,LEFT)
	(font-series . bold)
	(padding . 1.0)
	(side-axis . ,X)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				side-position-interface
				stanza-number-interface
				text-interface))))))

    (Stem
     . (
	(beamlet-default-length . (1.1 . 1.1))
	(beamlet-max-length-proportion . (0.75 . 0.75))
	(cross-staff . ,ly:stem::calc-cross-staff)
	(default-direction . ,ly:stem::calc-default-direction)
	(details
	 . (
	    ;; 3.5 (or 3 measured from note head) is standard length
	    ;; 32nd, 64th, 128th flagged stems should be longer
	    (lengths . (3.5 3.5 3.5 4.5 5.0 6.0))

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

	;; We use the normal minima as minimum for the ideal lengths,
	;; and the extreme minima as abolute minimum length.

	(direction . ,ly:stem::calc-direction)
	(duration-log . ,stem::calc-duration-log)
	(flag . ,ly:stem::calc-flag)
	(length . ,ly:stem::calc-length)
	(neutral-direction . ,DOWN)
	(positioning-done . ,ly:stem::calc-positioning-done)
	(stem-end-position . ,ly:stem::calc-stem-end-position)
	(stem-info . ,ly:stem::calc-stem-info)
	(stencil . ,ly:stem::print)
	(thickness . 1.3)
	(X-extent . ,ly:stem::width)
	(X-offset . ,ly:stem::offset-callback)
	(Y-extent . ,ly:stem::height)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				stem-interface))))))

    (StemTremolo
     . (
	(beam-thickness . 0.48) ; staff-space
	(beam-width . ,ly:stem-tremolo::calc-width) ; staff-space
	(slope . ,ly:stem-tremolo::calc-slope)
	(stencil . ,ly:stem-tremolo::print)
	(style . ,ly:stem-tremolo::calc-style)
	(X-extent . ,ly:stem-tremolo::width)
	(Y-extent . ,ly:stem-tremolo::height)
	(meta . ((class . Item)
		 (interfaces . (stem-tremolo-interface))))))

    (StringNumber
     . (
	(avoid-slur . around)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(padding . 0.5)
	(script-priority . 100)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(staff-padding . 0.5)
	(stencil . ,print-circled-text-callback)
	(text . ,string-number::calc-text)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				string-number-interface
				text-interface
				text-script-interface))))))

    (StrokeFinger
     . (
	(digit-names . #("p" "i" "m" "a" "x"))
	(font-shape . italic)
	(font-size . -4) 		; don't overlap when next to heads.
	(padding . 0.5)
	(script-priority . 100)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(staff-padding . 0.5)
	(stencil . ,ly:text-interface::print)
	(text . ,stroke-finger::calc-text)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				self-alignment-interface
				side-position-interface
				stroke-finger-interface
				text-interface
				text-script-interface))))))

    (SustainPedal
     . (
	(direction . ,RIGHT)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(padding . 0.0)  ;; padding relative to SustainPedalLineSpanner
	(self-alignment-X . ,CENTER)
	(stencil . ,ly:sustain-pedal::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				piano-pedal-interface
				piano-pedal-script-interface
				self-alignment-interface
				text-interface))))))

    (SustainPedalLineSpanner
     . (
	(axes . (,Y))
	(direction . ,DOWN)
	(minimum-space . 1.0)
	(outside-staff-priority . 1000)
	(padding . 1.2)
	(side-axis . ,Y)
	(staff-padding . 1.2)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				piano-pedal-interface
				side-position-interface))))))

    (System
     . (
	(axes . (,X ,Y))
	(vertical-skylines . ,ly:axis-group-interface::calc-skylines)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . System)
		 (interfaces . (axis-group-interface
				system-interface))))))

    (SystemStartBar
     . (
	(collapse-height . 5.0)
	(direction . ,LEFT)

	;; ugh--hardcoded.
	(padding . -0.1)  ;; bar must cover rounded ending of staff line.
	(stencil . ,ly:system-start-delimiter::print)
	(style . bar-line)
	(thickness . 1.6)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				system-start-delimiter-interface))))))

    (SystemStartBrace
     . (
	(collapse-height . 5.0)
	(direction . ,LEFT)
	(font-encoding . fetaBraces)
	(padding . 0.3)
	(stencil . ,ly:system-start-delimiter::print)
	(style . brace)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				side-position-interface
				system-start-delimiter-interface))))))

    (SystemStartBracket
     . (
	(collapse-height . 5.0)
	(direction . ,LEFT)
	(padding . 0.8)
	(stencil . ,ly:system-start-delimiter::print)
	(style . bracket)
	(thickness . 0.45)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				side-position-interface
				system-start-delimiter-interface))))))

    (SystemStartSquare
     . (
	(direction . ,LEFT)
	(stencil . ,ly:system-start-delimiter::print)
	(style . line-bracket)
	(thickness . 1.0)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				side-position-interface
				system-start-delimiter-interface))))))


    (TabNoteHead
     . (
	(direction . ,CENTER)
	(duration-log . ,note-head::calc-duration-log)
	(font-series . bold)
	(font-size . -2)
	(stem-attachment . (0.0 . 1.35))
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces  . (font-interface
				 note-head-interface
				 rhythmic-grob-interface
				 rhythmic-head-interface
				 staff-symbol-referencer-interface
				 text-interface))))))

    (TextScript
     . (
	(avoid-slur . around)
	(cross-staff . ,ly:script-interface::calc-cross-staff)
	(direction . ,DOWN)
	(direction . ,ly:script-interface::calc-direction)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(outside-staff-priority . 450)

	;; sync with Fingering ?
	(padding . 0.5)

	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(script-priority . 200)
	(side-axis . ,Y)
	(slur-padding . 0.5)
	(staff-padding . 0.5)
	(stencil . ,ly:text-interface::print)
	;; todo: add X self alignment?
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				instrument-specific-markup-interface
				self-alignment-interface
				side-position-interface
				text-interface
				text-script-interface))))))

    (TextSpanner
     . (
	(bound-details . ((left . ((Y . 0)
				   (padding . 0.25)
				   (attach-dir . ,LEFT)
				   ))
			  (right . ((Y . 0)
				    (padding . 0.25)
				    ))
			  ))
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(direction . ,UP)
	(font-shape . italic)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(outside-staff-priority . 350)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(side-axis . ,Y)
	(staff-padding . 0.8)
	(stencil . ,ly:line-spanner::print)
	(style . dashed-line)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)

	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				line-interface
				line-spanner-interface
				side-position-interface))))))

    (Tie
     . (
	(avoid-slur . inside)
	(control-points . ,ly:tie::calc-control-points)
	(details . (
		    ;; for a full list, see tie-details.cc
		    (ratio . 0.333)
		    (center-staff-line-clearance . 0.6)
		    (tip-staff-line-clearance . 0.45)
		    (note-head-gap . 0.2)
		    (stem-gap . 0.35)
		    (height-limit . 1.0)
		    (horizontal-distance-penalty-factor . 10)
		    (same-dir-as-stem-penalty . 8)
		    (min-length-penalty-factor . 26)
		    (tie-tie-collision-distance . 0.45)
		    (tie-tie-collision-penalty . 25.0)
		    (intra-space-threshold . 1.25)
		    (outer-tie-vertical-distance-symmetry-penalty-factor . 10)
		    (outer-tie-length-symmetry-penalty-factor . 10)
		    (vertical-distance-penalty-factor . 7)
		    (outer-tie-vertical-gap . 0.25)
		    (multi-tie-region-size . 3)
		    (single-tie-region-size . 4)
		    (between-length-limit . 1.0)))

	(direction . ,ly:tie::calc-direction)
	(font-size . -6)
	(line-thickness . 0.8)
	(neutral-direction . ,UP)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:tie::print)
	(thickness . 1.2)
	(meta . ((class . Spanner)
		 (interfaces . (tie-interface))))))

    (TieColumn
     . (
	(before-line-breaking . ,ly:tie-column::before-line-breaking)
	(positioning-done . ,ly:tie-column::calc-positioning-done)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (tie-column-interface))))))

    (TimeSignature
     . (
	(avoid-slur . inside)
	(break-align-anchor
	 . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-align-symbol . time-signature)
	(break-visibility . ,all-visible)
	(extra-spacing-height . (-1.0 . 1.0))
	(non-musical . #t)
	(space-alist . (
			(first-note . (fixed-space . 2.0))
			(right-edge . (extra-space . 0.5))
			(staff-bar . (minimum-space . 2.0))))
	(stencil . ,ly:time-signature::print)
	(style . C)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				font-interface
				time-signature-interface))))))

    (TrillPitchAccidental
     . (
	(direction . ,LEFT)
	(font-size . -4)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(padding . 0.2)
	(side-axis . ,X)
	(stencil . ,ly:accidental-interface::print)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(Y-extent . ,ly:accidental-interface::height)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface
				side-position-interface
				trill-pitch-accidental-interface))))))

    (TrillPitchGroup
     . (
	(axes . (,X))
	(direction . ,RIGHT)
	(font-size . -4)
	(padding . 0.3)
	(side-axis . ,X)
	(stencil . ,parenthesize-elements)
	(stencils . ,parentheses-item::calc-parenthesis-stencils)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				font-interface
				note-head-interface
				parentheses-interface
				side-position-interface))))))

    (TrillPitchHead
     . (
	(duration-log . 2)
	(font-size . -4)
	(stencil . ,ly:note-head::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				ledgered-interface
				pitched-trill-interface
				rhythmic-head-interface
				staff-symbol-referencer-interface))))))

    (TrillSpanner
     . (
	(after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
	(bound-details . ((left . ((text . ,(make-translate-scaled-markup
					     '(0.0 . -1.0)
					     (make-musicglyph-markup "scripts.trill")))
				   (Y . 0)
				   (stencil-offset . (-0.5 . 0))
				   (padding . 1.5)
				   (attach-dir . ,CENTER)
				   ;; this isn't CENTER because the trill glyph's origin
				   ;; is not centered in its extent; to have the trill
				   ;; spanner aligned the same as a trill, we need a slight offset
				   (anchor-alignment . 0.15)
				   ))
			  (left-broken . ((end-on-note . #t)))
			  (right . ((Y . 0)))
			  ))
	(direction . ,UP)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(outside-staff-priority . 50)
	(padding . 0.5)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(side-axis . ,Y)
	(staff-padding . 1.0)
	(stencil . ,ly:line-spanner::print)
	(style . trill)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				line-interface
				line-spanner-interface
				side-position-interface
				trill-spanner-interface))))))

    (TupletBracket
     . (
	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	(control-points . ,ly:tuplet-bracket::calc-control-points)
	(cross-staff . ,ly:tuplet-bracket::calc-cross-staff)
	(direction  . ,ly:tuplet-bracket::calc-direction)
	(edge-height . (0.7 . 0.7))
	(full-length-to-extent . #t)
	(padding . 1.1)
	(positions . ,ly:tuplet-bracket::calc-positions)
	(shorten-pair . (-0.2 . -0.2))
	(staff-padding . 0.25)
	(stencil . ,ly:tuplet-bracket::print)
	(thickness . 1.6)

	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				tuplet-bracket-interface))))))

    (TupletNumber
     . (
	(avoid-slur . inside)
	(cross-staff . ,ly:tuplet-number::calc-cross-staff)
	(font-shape . italic)
	(font-size . -2)
	(stencil . ,ly:tuplet-number::print)
	(text . ,tuplet-number::calc-denominator-text)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				text-interface
				tuplet-number-interface))))))


    (UnaCordaPedal
     . (
	(direction . ,RIGHT)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(font-shape . italic)
	(padding . 0.0)  ;; padding relative to UnaCordaPedalLineSpanner
	(self-alignment-X . ,CENTER)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				piano-pedal-script-interface
				self-alignment-interface
				text-interface))))))

    (UnaCordaPedalLineSpanner
     . (
	(axes . (,Y))
	(direction . ,DOWN)
	(minimum-space . 1.0)
	(outside-staff-priority . 1000)
	(padding . 1.2)
	(side-axis . ,Y)
	(staff-padding . 1.2)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				piano-pedal-interface
				side-position-interface))))))


    (VaticanaLigature
     . (
	(flexa-width . 2.0)
	(stencil . ,ly:vaticana-ligature::print)
	(thickness . 0.6)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				vaticana-ligature-interface))))))

    (VerticalAlignment
     . (
	(axes . (,Y))
	(positioning-done . ,ly:align-interface::align-to-ideal-distances)
	(stacking-dir . -1)
	(vertical-skylines . ,ly:axis-group-interface::combine-skylines)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(meta . ((class . Spanner)
		 (object-callbacks . ((Y-common . ,ly:axis-group-interface::calc-y-common)))
		 (interfaces . (align-interface
				axis-group-interface))))))

    (VerticalAxisGroup
     . (
	(adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
	(axes . (,Y))
	(default-next-staff-spacing . ((space . 9) (minimum-distance . 8)))
	(next-staff-spacing . ,ly:axis-group-interface::calc-next-staff-spacing)
	(stencil . ,ly:axis-group-interface::print)
	(vertical-skylines . ,ly:hara-kiri-group-spanner::calc-skylines)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:hara-kiri-group-spanner::y-extent)
	(Y-offset . ,ly:hara-kiri-group-spanner::force-hara-kiri-callback)
	(meta . ((class . Spanner)
		 (object-callbacks . ((X-common . ,ly:axis-group-interface::calc-x-common)))
		 (interfaces . (axis-group-interface
				hara-kiri-group-spanner-interface
				vertically-spaceable-interface))))))

    (VoiceFollower
     . (
	(after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
	(bound-details . ((right . ((attach-dir .  ,CENTER)
				    (padding . 1.5)
				      ))
			  (left . ((attach-dir .  ,CENTER)
				   (padding . 1.5)
				      ))
			  ))
	(gap . 0.5)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(non-musical . #t)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(stencil . ,ly:line-spanner::print)
	(style . line)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				line-spanner-interface))))))

    (VoltaBracket
     . (
	(direction . ,UP)
	(edge-height . (2.0 . 2.0)) ;; staff-space;
	(font-encoding . fetaNumber)
	(font-size . -4)
	(stencil . ,ly:volta-bracket-interface::print)
	(thickness . 1.6)  ;;  line-thickness
	(word-space . 0.6)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				horizontal-bracket-interface
				line-interface
				side-position-interface
				text-interface
				volta-bracket-interface))))))

    (VoltaBracketSpanner
     . (
	(axes . (,Y))
	(direction . ,UP)
	(no-alignment . ,#t)
	(outside-staff-priority . 100)
	(padding . 1)
	(side-axis . ,Y)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				side-position-interface))))))

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

    (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
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

(define pure-print-callbacks
  (list
   fret-board::calc-stencil
   print-circled-text-callback
   lyric-text::print
   ly:arpeggio::print
   ly:arpeggio::brew-chord-bracket
   ly:bar-line::print
   ly:mensural-ligature::brew-ligature-primitive
   ly:note-head::print
   ly:dots::print
   ly:clef::print
   ly:text-interface::print
   ly:script-interface::print))

;; ly:grob::stencil-extent is safe iff the print callback is safe too
(define (pure-stencil-height grob start stop)
  (let ((sten (ly:grob-property-data grob 'stencil)))
    (if (or
	 (ly:stencil? sten)
	 (memq sten pure-print-callbacks))
	(ly:grob::stencil-height grob)
	'(0 . 0))))

(define pure-conversions-alist
  `(
    (,ly:accidental-interface::height . ,ly:accidental-interface::pure-height)
    (,ly:arpeggio::height . ,ly:arpeggio::pure-height)
    (,ly:slur::outside-slur-callback . ,ly:slur::pure-outside-slur-callback)
    (,ly:hairpin::height . ,ly:hairpin::pure-height)
    (,ly:stem::height . ,ly:stem::pure-height)
    (,ly:rest::height . ,ly:rest::pure-height)
    (,ly:grob::stencil-height . ,pure-stencil-height)
    (,ly:self-alignment-interface::y-aligned-on-self . ,ly:self-alignment-interface::pure-y-aligned-on-self)
    (,ly:side-position-interface::y-aligned-side . ,ly:side-position-interface::pure-y-aligned-side)
    (,ly:axis-group-interface::height . ,ly:axis-group-interface::pure-height)
    (,ly:hara-kiri-group-spanner::y-extent . ,ly:hara-kiri-group-spanner::pure-height)
    (,ly:slur::height . ,ly:slur::pure-height)
    (,ly:side-position-interface::y-aligned-side . ,ly:side-position-interface::pure-y-aligned-side)))

(define pure-functions
  (list
   parenthesize-elements
   ly:rest::y-offset-callback
   ly:staff-symbol-referencer::callback
   ly:staff-symbol::height))

(define-public (pure-relevant? grob)
  (let ((extent-callback (ly:grob-property-data grob 'Y-extent)))
    (not (eq? #f
	      (or
	       (pair? extent-callback)
	       (memq extent-callback pure-functions)
	       (and
		(pair? (assq extent-callback pure-conversions-alist))
		(begin
		  (or
		   (not (eq? extent-callback ly:grob::stencil-height))
		   (memq (ly:grob-property-data grob 'stencil) pure-print-callbacks)
		   (ly:stencil? (ly:grob-property-data grob 'stencil))

		   ))))))))

(define-public (call-pure-function unpure args start end)
  (if (ly:simple-closure? unpure)
      (ly:eval-simple-closure (car args) unpure start end)
      (if (not (procedure? unpure))
	  unpure
	  (if (memq unpure pure-functions)
	      (apply unpure args)
	      (let ((pure (assq unpure pure-conversions-alist)))
		(if pure
		    (apply (cdr pure)
			   (append
			    (list (car args) start end)
			    (cdr args)))))))))
