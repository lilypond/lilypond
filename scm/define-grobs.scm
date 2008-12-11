;;;; define-grobs.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; distances are given in line-thickness (thicknesses) and
;;;; staff-space (distances)

;;;; WARNING: the meta field should be the last one.
;;;; WARNING: don't use anonymous functions for initialization.

;; TODO: junk the meta field in favor of something more compact?

;;; todo:: reorder sensibly.

(define-public all-grob-descriptions
  `(
    (Accidental
     . (
	(avoid-slur . inside)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(alteration . ,accidental-interface::calc-alteration)
	(stencil . ,ly:accidental-interface::print)
	(Y-extent . ,ly:accidental-interface::height)
	(X-extent . ,ly:accidental-interface::width)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface))))))

    (AccidentalCautionary
     . (
	(avoid-slur . inside)
	(parenthesized . #t)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(alteration . ,accidental-interface::calc-alteration)
	(stencil . ,ly:accidental-interface::print)
	(Y-extent . ,ly:accidental-interface::height)
	(meta . ((class . Item)
		 (interfaces . (accidental-interface
				font-interface))))))

    (AccidentalSuggestion
     . (
	(stencil . ,ly:accidental-interface::print)
        (X-extent . ,ly:accidental-interface::width)
	(Y-extent . ,ly:accidental-interface::height)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(self-alignment-X . ,CENTER)
	(font-size . -2)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(alteration . ,accidental-interface::calc-alteration)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(direction . ,UP)
	(staff-padding . 0.25)
	(outside-staff-priority . 0)
	(script-priority . 0)
	(side-axis . ,Y)
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

	;; for horizontally stacked scripts.
	(script-priority .  -100)
	(direction .  ,LEFT)
	
	(positioning-done . ,ly:accidental-placement::calc-positioning-done)
	(X-extent . ,ly:axis-group-interface::width)		      
	
	;; this is quite small, but it is very ugly to have
	;; accs closer to the previous note than to the next one.
	(right-padding . 0.15)
	(meta . ((class . Item)
		 (interfaces . (accidental-placement-interface))))))
    
    (Ambitus
     . (
	(axes . (,X ,Y))
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(space-alist . (
			(clef . (extra-space . 0.5))
			(key-signature . (extra-space . 0.0))
			(staff-bar . (extra-space . 0.0))
			(time-signature . (extra-space . 0.0))
			(first-note . (fixed-space . 0.0))))
	(non-musical . #t)
	(break-align-symbol . ambitus)
	(break-visibility . ,begin-of-line-visible)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				break-aligned-interface
				ambitus-interface))))))

    (AmbitusLine
     . (
	(stencil . ,ly:ambitus::print)
	(join-heads . #t)
	(thickness . 2)
	(X-offset . ,ly:self-alignment-interface::centered-on-x-parent)
	(meta . ((class . Item)
		 (interfaces . (ambitus-interface
				staff-symbol-referencer-interface
				font-interface))))))
    (AmbitusAccidental
     . (
	(font-family . music)
	(padding . 0.5)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(stencil . ,ly:accidental-interface::print)
	(Y-extent . ,ly:accidental-interface::height)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)	
	(side-axis . ,X)
	(meta . ((class . Item)
		 (interfaces . (
				accidental-interface
				break-aligned-interface
				side-position-interface
				font-interface))))))

    (AmbitusNoteHead
     . (
	(duration-log . 2)
	(stencil . ,ly:note-head::print)
	(glyph-name . ,note-head::calc-glyph-name)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				note-head-interface
				ambitus-interface
				staff-symbol-referencer-interface
				rhythmic-head-interface
				ledgered-interface))))))

    (Arpeggio
     . ((X-extent . ,ly:arpeggio::width)
	(stencil . ,ly:arpeggio::print)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(positions . ,ly:arpeggio::calc-positions)
	(padding . 0.5)
	(script-priority . 0)
	(side-axis . ,X)
	(staff-position . 0.0)
	(Y-extent . ,ly:arpeggio::height)
	(meta . ((class . Item)
		 (interfaces . (arpeggio-interface
				staff-symbol-referencer-interface
				side-position-interface
				font-interface))))))

    (BalloonTextItem 
     . ((stencil . ,ly:balloon-interface::print)
	(text . ,(grob::calc-property-by-copy 'text)) 
	(X-offset . ,(grob::calc-property-by-copy 'X-offset)) 
	(Y-offset . ,(grob::calc-property-by-copy 'Y-offset)) 
	(meta . ((class . Item)
		 (interfaces . (balloon-interface
				text-interface
				font-interface))))))
    (BarLine
     . (
	(break-align-symbol . staff-bar)
	(break-align-anchor . ,ly:bar-line::calc-anchor)
	(glyph . "|")
	(gap . 0.4)
	(layer . 0)
	(break-visibility . ,bar-line::calc-break-visibility)
	(non-musical . #t)
	(stencil . ,ly:bar-line::print)
	(glyph-name . ,bar-line::calc-glyph-name)
	(bar-size .  ,ly:bar-line::calc-bar-size)
	(bar-extent . ,ly:bar-line::calc-bar-extent)
	(allow-span-bar . #t)
	
	(space-alist . (
			(time-signature . (extra-space . 0.75))
			(custos . (minimum-space . 2.0))
			(clef . (minimum-space . 1.0))
			(key-signature . (extra-space . 1.0))
			(key-cancellation . (extra-space . 1.0))
			(first-note . (fixed-space . 1.3))
			(next-note . (semi-fixed-space . 0.9))
			(right-edge . (extra-space . 0.0))))

	;;
	;; Ross. page 151 lists other values, we opt for a leaner look
	;;
	;; TODO:
	;; kern should scale with line-thickness too.
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
	(stencil . ,ly:text-interface::print)
	(non-musical . #t)
	(break-visibility . ,begin-of-line-visible)
	(padding . 1.0)
	(direction . ,UP)
	(font-family . roman)
	(font-size . -2)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(outside-staff-priority . 100)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:break-alignable-interface::self-align-callback))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))

	(self-alignment-X . ,RIGHT)

	;; want the bar number before the clef at line start. 
	(break-align-symbols . (left-edge staff-bar))
	(meta .
	      ((class . Item)
	       (interfaces . (side-position-interface
			      text-interface
			      break-alignable-interface
			      self-alignment-interface
			      font-interface))))))

    (BassFigure
     . (
	(stencil . ,ly:text-interface::print)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				rhythmic-grob-interface
				bass-figure-interface
				font-interface))))))

    (BassFigureAlignment
     . (
	(axes . (,Y))
	(threshold . (2 . 1000))
	(positioning-done . ,ly:align-interface::calc-positioning-done)
	(Y-extent . ,ly:axis-group-interface::height)
	(stacking-dir . ,DOWN)
	(padding . 0.2)
	(meta . ((class . Spanner)
		 (interfaces . (align-interface
				bass-figure-alignment-interface
				axis-group-interface))))))

    (BassFigureAlignmentPositioning
     . ((Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(direction . ,UP)
	(Y-extent . ,ly:axis-group-interface::height)
	(axes . (,Y))
	(staff-padding . 1.0)
	(padding . 0.5)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				axis-group-interface))))))
    
    (BassFigureBracket
     . (
	(stencil . ,ly:enclosing-bracket::print)
	(X-extent . ,ly:enclosing-bracket::width)
	(edge-height . (0.2 . 0.2))
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
	(axes . (,Y))
	(Y-extent . ,ly:axis-group-interface::height)
	(vertical-skylines . ,ly:axis-group-interface::calc-skylines)
	(adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface))))))


    (Beam
     . (
	;; todo: clean this up a bit: the list is getting
	;; rather long.
	(gap . 0.8)
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
	(concaveness . ,ly:beam::calc-concaveness)
	(direction . ,ly:beam::calc-direction)
	(shorten . ,ly:beam::calc-stem-shorten)
	(beaming . ,ly:beam::calc-beaming)
	(stencil . ,ly:beam::print)
	(clip-edges . #t)
	(cross-staff . ,ly:beam::calc-cross-staff)

	(details .  ((hint-direction-penalty . 20)))
	;; TODO: should be in SLT.
	(thickness . 0.48) ; in staff-space
	(neutral-direction . ,DOWN)

	;; Whe have some unreferenced problems here.
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
	(damping . 1)
	(auto-knee-gap . 5.5)

	;; only for debugging.
	(font-family . roman)
	(meta . ((class . Spanner)
		 (object-callbacks . ((normal-stems . ,ly:beam::calc-normal-stems))) 
		 (interfaces . (staff-symbol-referencer-interface
				unbreakable-spanner-interface
				beam-interface
				font-interface))))))

    (BendAfter
     . (
	(stencil . ,bend::print)
	(thickness . 2.0)
	(meta . ((class . Spanner)
		 (interfaces . (spanner-interface
				bend-after-interface))))))

    (BreakAlignment
     . (
	(non-musical . #t)
	(stacking-dir . 1)
	(positioning-done . ,ly:break-alignment-interface::calc-positioning-done)
	(X-extent . ,ly:axis-group-interface::width)
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
			       staff
			       time-signature custos)

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
	(axes . (,X))
	(meta . ((class . Item)
		 (interfaces . (break-alignment-interface
				axis-group-interface))))))

    (BreakAlignGroup
     . (
	(axes . (,X))
	(X-extent . ,ly:axis-group-interface::width)
	(break-align-anchor . ,ly:break-aligned-interface::calc-average-anchor)
	(break-visibility . ,ly:break-aligned-interface::calc-break-visibility)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				axis-group-interface))))))

    (BreathingSign
     . (
	(break-align-symbol . breathing-sign)
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
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((class . Item)
		 (interfaces . (break-aligned-interface
				breathing-sign-interface
				text-interface
				font-interface))))))

    (Clef
     . (
	(stencil . ,ly:clef::print)
	(glyph-name . ,ly:clef::calc-glyph-name)
	(non-musical . #t)
	(avoid-slur . inside)
	(font-family . music)
	(break-align-symbol . clef)
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-visibility . ,begin-of-line-visible)
	(space-alist . ((ambitus . (extra-space . 2.0))
			(staff-bar . (extra-space . 0.7))
			(key-cancellation . (minimum-space . 3.5))
			(key-signature . (minimum-space . 3.5))
			(time-signature . (minimum-space . 4.2))
			(first-note . (minimum-fixed-space . 5.0))
			(next-note . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))))
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (clef-interface
				staff-symbol-referencer-interface
				font-interface
				break-aligned-interface))))))

    (ClusterSpannerBeacon
     . (
	(Y-extent . ,ly:cluster-beacon::height)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				cluster-beacon-interface))))))

    (ClusterSpanner
     . (
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(stencil . ,ly:cluster::print)
	(minimum-length . 0.0)
	(padding . 0.25)
	(cross-staff . ,ly:cluster::calc-cross-staff)
	(style . ramp)
	(meta . ((class . Spanner)
		 (interfaces . (cluster-interface))))))

    (ChordName
     . (
	(stencil . ,ly:text-interface::print)
	(after-line-breaking . ,ly:chord-name::after-line-breaking)
	(word-space . 0.0)
	(font-family . sans)
	(font-size . 1.5)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				rhythmic-grob-interface
				text-interface
				chord-name-interface))))))

    (CombineTextScript
     . (
	(stencil . ,ly:text-interface::print)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(direction . ,UP)
	(padding . 0.5)
	(staff-padding . 0.5)
	(script-priority . 200)
	;; todo: add X self alignment?
	(baseline-skip . 2)
	(side-axis . ,Y)
	(avoid-slur . outside )
	(font-series . bold)
	(meta . ((class . Item)
		 (interfaces . (text-script-interface
				text-interface
				side-position-interface
				font-interface))))))

    (Custos
     . (
	(break-align-symbol . custos)
	(non-musical . #t)
	(stencil . ,ly:custos::print)
	(break-visibility . ,end-of-line-visible)
	(style . vaticana)
	(neutral-direction . ,DOWN)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(space-alist . (
			(first-note . (minimum-fixed-space . 0.0))
			(right-edge . (extra-space . 0.1))))
	(meta . ((class . Item)
		 (interfaces  . (custos-interface
				 staff-symbol-referencer-interface
				 font-interface
				 break-aligned-interface))))))

    (DotColumn
     . (
	(axes . (,X))
	(direction . ,RIGHT)
	(positioning-done . ,ly:dot-column::calc-positioning-done) 
	(X-extent . ,ly:axis-group-interface::width)
	(meta . ((class . Item)
		 (interfaces . (dot-column-interface
				axis-group-interface))))))

    (Dots
     . (
	(stencil . ,ly:dots::print)
	(dot-count . ,dots::calc-dot-count)
	(staff-position . ,dots::calc-staff-position)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				staff-symbol-referencer-interface
				dots-interface))))))

    (DoublePercentRepeat
     . (
	(stencil . ,ly:percent-repeat-item-interface::double-percent)
	(non-musical . #t)
	(slope . 1.0)
	(dot-negative-kern . 0.75)
	(slash-negative-kern . 1.6)
	(font-encoding . fetaMusic)
	(width . 2.0)
	(thickness . 0.48)
	(break-align-symbol . staff-bar)
	(break-visibility . ,begin-of-line-invisible)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				break-aligned-interface
				percent-repeat-interface
				percent-repeat-item-interface))))))

    (DoublePercentRepeatCounter
     . (
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(font-encoding . fetaNumber)
	(self-alignment-X . ,CENTER)
	(font-size . -2) 
	(direction . ,UP)
	(padding . 0.2)
	(staff-padding . 0.25)
	(side-axis . ,Y)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				self-alignment-interface
				percent-repeat-item-interface
				percent-repeat-interface
				font-interface
				text-interface))))))

    (DynamicLineSpanner
     . (
	(axes . (,Y))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(staff-padding . 0.1)
	(padding . 0.6)
	(slur-padding . 0.3)
	(minimum-space . 1.2)
	(direction . ,DOWN)
	(side-axis . ,Y)
	(outside-staff-priority . 250)
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(cross-staff . ,ly:side-position-interface::calc-cross-staff)
	(meta . ((class . Spanner)
		 (interfaces . (axis-group-interface
				dynamic-interface
				dynamic-line-spanner-interface
				side-position-interface))))))

    (DynamicText
     . (

	;; todo.

	(stencil . ,ly:text-interface::print)
	(direction . ,ly:script-interface::calc-direction)
	(positioning-done . ,ly:script-interface::calc-positioning-done)

	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(self-alignment-X . ,CENTER)
	(Y-offset . ,ly:self-alignment-interface::y-aligned-on-self)
	(self-alignment-Y . ,CENTER)
	(font-series . bold)
	(font-encoding . fetaDynamic)
	(font-shape . italic)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(outside-staff-priority . 250)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				text-interface
				self-alignment-interface
				dynamic-interface
				script-interface))))))

    (DynamicTextSpanner
     . (
	;; rather ugh with NCSB
	;; (font-series . bold)
	(font-shape . italic)
	(style . dashed-line)

	;; make sure the spanner doesn't get too close to notes
	(minimum-Y-extent . (-1 . 1))
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
	(stencil . ,ly:line-spanner::print)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info-and-text)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)

	;; need to blend with dynamic texts.
	(font-size . 1)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(meta . ((class . Spanner)
		 (interfaces . (font-interface
				text-interface
				line-spanner-interface
				line-interface
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
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(script-priority . 100)
	(stencil . ,ly:text-interface::print)
	(direction . ,ly:script-interface::calc-direction)
	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(cross-staff . ,ly:side-position-interface::calc-cross-staff)

	(text . ,fingering::calc-text) 
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((class . Item)
		 (interfaces . (finger-interface
				font-interface
				text-script-interface
				text-interface
				side-position-interface
				self-alignment-interface))))))

    (FretBoard
     . ((stencil . ,fret-board::calc-stencil)
	(fret-diagram-details . ((finger-code . below-string)))
	(meta . ((class . Item)
		 (interfaces . (fret-diagram-interface
				font-interface))))))


    (Glissando
     . (
	(style . line)
	(gap . 0.5)
	(zigzag-width . 0.75)
	(X-extent . #f)
	(Y-extent . #f)
	(bound-details . ((right . ((attach-dir .  ,CENTER)
				    (padding . 1.5)
				      ))
			  (left . ((attach-dir .  ,CENTER)
				   (padding . 1.5)
				      ))
			  ))
	(stencil . ,ly:line-spanner::print)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				unbreakable-spanner-interface
				line-spanner-interface))))))

    (GraceSpacing
     . (
	(common-shortest-duration . ,grace-spacing::calc-shortest-duration)
	(spacing-increment . 0.8)
	(shortest-duration-space . 1.6)
	(meta . ((class . Spanner)
		 (interfaces . (grace-spacing-interface
				spacing-options-interface
				spanner-interface))))))

    (GridPoint
     . (
	(X-extent . (0 . 0))
	(Y-extent . (0 . 0))
	(meta . ((class . Item)
		 (interfaces . (grid-point-interface))))))

    (GridLine
     . (
	(X-extent  . ,ly:grid-line-interface::width)
	(stencil . ,ly:grid-line-interface::print)
	(self-alignment-X . ,CENTER)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(layer . 0)
	(meta . ((class . Item)
		 (interfaces . (self-alignment-interface
				grid-line-interface))))))


    (Hairpin
     . (
	(stencil . ,ly:hairpin::print)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(after-line-breaking . ,ly:hairpin::after-line-breaking)
	(grow-direction . ,hairpin::calc-grow-direction)
	(circled-tip . #f)
	(to-barline . #t)
	(thickness . 1.0)
	(height . 0.6666)
	(minimum-length . 2.0)
	(bound-padding . 1.0)
	(self-alignment-Y . ,CENTER)
	(Y-offset . ,ly:self-alignment-interface::y-aligned-on-self)
	(meta . ((class . Spanner)
		 (interfaces . (hairpin-interface
				line-interface
				self-alignment-interface
				dynamic-interface
				spanner-interface))))))

    (HorizontalBracket
     . (
	(thickness . 1.0)
	(stencil . ,ly:horizontal-bracket::print)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	
	(padding . 0.2)
	(staff-padding . 0.2)
	(direction . ,DOWN)
	(side-axis . ,Y)
	(bracket-flare . (0.5 . 0.5))
	(meta . ((class . Spanner)
		 (interfaces . (horizontal-bracket-interface
				side-position-interface
				line-interface
				spanner-interface))))))


    (InstrumentName
     . (
	(padding . 0.3)
	(stencil . ,ly:system-start-text::print)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(self-alignment-Y . ,CENTER)
	(self-alignment-X . ,CENTER)
	(meta . ((class . Spanner)
		 (interfaces . (system-start-text-interface
				side-position-interface
				self-alignment-interface
				font-interface))))))

    (InstrumentSwitch
     . (
	(padding . 0.5)
	(stencil . ,ly:text-interface::print)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(staff-padding . 0.5)
	(direction . ,UP)
	(side-axis . ,Y)
	(self-alignment-X . ,LEFT)
	(outside-staff-priority . 500)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				self-alignment-interface
				text-interface
				font-interface))))))

    
    (KeyCancellation
     . (
	(stencil . ,ly:key-signature-interface::print)
	(glyph-name-alist . ,cancellation-glyph-name-alist)
	(space-alist . (
			(time-signature . (extra-space . 1.25))
			(staff-bar . (extra-space . 0.6))
			(key-signature . (extra-space . 0.5))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(break-align-symbol . key-cancellation)
	(break-visibility . ,begin-of-line-invisible)
	(non-musical . #t)
	(meta . ((class . Item)
		 (interfaces . (key-cancellation-interface
				key-signature-interface
				staff-symbol-referencer-interface
				font-interface
				break-aligned-interface))))))

    (KeySignature
     . (
	(stencil . ,ly:key-signature-interface::print)
	(avoid-slur . inside)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(space-alist . (
			(time-signature . (extra-space . 1.15))
			(staff-bar . (extra-space . 1.1))
			(right-edge . (extra-space . 0.5))
			(first-note . (fixed-space . 2.5))))
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(break-align-symbol . key-signature)
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-visibility . ,begin-of-line-visible)
	(non-musical . #t)
	(meta . ((class . Item)
		 (interfaces . (key-signature-interface
				staff-symbol-referencer-interface
				font-interface
				break-aligned-interface))))))
    
    
   (LaissezVibrerTie
     . (
	(stencil  . ,ly:tie::print)
	(control-points . ,ly:semi-tie::calc-control-points)
	(direction . ,ly:tie::calc-direction)
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)))
	(head-direction . ,LEFT)
	(thickness . 1.0)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-interface))))))

    (LaissezVibrerTieColumn
     . (
	(X-extent . #f)
	(Y-extent . #f)
	(head-direction . ,LEFT)
	(positioning-done . ,ly:semi-tie-column::calc-positioning-done)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-column-interface))))))

    (LedgerLineSpanner
     . (
	(springs-and-rods . ,ly:ledger-line-spanner::set-spacing-rods)
	(stencil . ,ly:ledger-line-spanner::print)
	(X-extent . #f)
	(Y-extent . #f)
	(minimum-length-fraction . 0.25)
	(length-fraction . 0.25)
	(layer . 0)
	(meta . ((class . Spanner)
		 (interfaces . (ledger-line-spanner-interface))))))

    (LeftEdge
     . (
	(break-align-symbol . left-edge)
	(break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(X-extent . (0 . 0))
	(non-musical . #t)
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
	(direction . ,UP)
	(positions . ,ly:tuplet-bracket::calc-positions)
	(stencil . ,ly:tuplet-bracket::print)
	(staff-padding . 0.25)
	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	(control-points . ,ly:tuplet-bracket::calc-control-points)
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				tuplet-bracket-interface))))))

    (LyricExtender
     . (
	(stencil . ,ly:lyric-extender::print)
	(thickness . 0.8) ; line-thickness
	(minimum-length . 1.5)
	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (lyric-interface
				lyric-extender-interface))))))

    (LyricHyphen
     . (
	(thickness . 1.3)
	(height . 0.42)
	(dash-period . 10.0)
	(length . 0.66)
	(minimum-length . 0.3)
	(minimum-distance . 0.1)
	(padding . 0.07)
	(springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
	(stencil . ,ly:lyric-hyphen::print)
	(Y-extent . (0 . 0))
	(meta . ((class . Spanner)
		 (interfaces . (lyric-interface
				font-interface
				lyric-hyphen-interface
				spanner-interface))))))

    (LyricSpace
     . ((minimum-distance . 0.45)
	(springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
	(padding . 0.0)
	(Y-extent . #f)
	(X-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (spanner-interface
				lyric-hyphen-interface))))))
    
    (LyricText
     . (
	(stencil . ,lyric-text::print)
	(text . ,(grob::calc-property-by-copy 'text)) 
	(X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
	(self-alignment-X . ,CENTER)
	(word-space . 0.6)
	(font-series . bold-narrow)
	(font-size . 1.0)
	(extra-spacing-width . (0.0 . 0.0))
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				lyric-syllable-interface
				self-alignment-interface
				text-interface
				font-interface))))))


    (MeasureGrouping
     . (
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(stencil . ,ly:measure-grouping::print)
	(padding . 2)
	(direction . ,UP)
	(thickness . 1)
	(height . 2.0)
	(staff-padding . 3)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				measure-grouping-interface))))))

    (MelodyItem
     . (
	(neutral-direction . ,DOWN)
	(meta . ((class . Item)
		 (interfaces . (melody-spanner-interface))))))

    (MensuralLigature
     . (
	(thickness . 1.4)
	(flexa-width . 2.0)
	(stencil . ,ly:mensural-ligature::print)
	(meta . ((class . Spanner)
		 (interfaces . (mensural-ligature-interface
				font-interface))))))

    (MetronomeMark
     . (
	(stencil . ,ly:text-interface::print)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(direction . ,UP)
	(padding . 0.8)
	(side-axis . ,Y)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(outside-staff-priority . 1000)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				side-position-interface
				font-interface
				metronome-mark-interface))))))

    (MultiMeasureRest
     . (
	(stencil . ,ly:multi-measure-rest::print)
	(springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
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
	(bound-padding  . 2.0)
	(springs-and-rods . ,ly:multi-measure-rest::set-text-rods)
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(self-alignment-X . ,CENTER)
	(direction . ,UP)
	(padding . 0.4)
	(staff-padding . 0.4)
	(font-encoding . fetaNumber)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				multi-measure-interface
				self-alignment-interface
				font-interface
				text-interface))))))

    (MultiMeasureRestText
     . (
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(self-alignment-X . ,CENTER)
	(direction . ,UP)
	(padding . 0.2)
	(staff-padding . 0.25)
	(outside-staff-priority . 450)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				multi-measure-interface
				self-alignment-interface
				font-interface
				text-interface))))))


    (NonMusicalPaperColumn
     . (
	(allow-loose-spacing . #t)
	(axes . (,X))
	(before-line-breaking . ,ly:paper-column::before-line-breaking)
	(X-extent . ,ly:axis-group-interface::width)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	;;		      (stencil . ,ly:paper-column::print)
	
	(non-musical . #t)
	(line-break-permission . allow)
	(page-break-permission . allow)

	;; debugging stuff: print column number.
	;;		 (font-size . -6) (font-name . "sans")	(Y-extent . #f)

	(meta . ((class . Paper_column)
		 (interfaces . (paper-column-interface
				axis-group-interface
				separation-item-interface
				spaceable-grob-interface
				font-interface))))))

    (NoteCollision
     . (
	(axes . (,X ,Y))
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(prefer-dotted-right . #t)
	(positioning-done . ,ly:note-collision-interface::calc-positioning-done)
	(meta . ((class . Item)
		 (interfaces . (note-collision-interface
				axis-group-interface))))))

    (NoteColumn
     . (
	(axes . (,X ,Y))
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	(meta . ((class . Item)
		 (interfaces . (axis-group-interface
				separation-item-interface
				note-column-interface))))))

    (NoteHead
     . (
	(stencil . ,ly:note-head::print)
	(duration-log . ,note-head::calc-duration-log)
	(stem-attachment . ,ly:note-head::calc-stem-attachment)
	(glyph-name . ,note-head::calc-glyph-name)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(X-offset . ,ly:note-head::stem-x-shift)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				rhythmic-head-interface
				font-interface
				note-head-interface
				ledgered-interface
				staff-symbol-referencer-interface
				gregorian-ligature-interface
				mensural-ligature-interface
				vaticana-ligature-interface))))))

    (NoteSpacing
     . (
	(stem-spacing-correction . 0.5)
	(same-direction-correction . 0.25)
	(space-to-barline . #t)
	;; Changed this from 0.75.
	;; If you ever change this back, please document! --hwn
	(knee-spacing-correction . 1.0)
	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				note-spacing-interface))))))

    (NoteName
     . (
	(stencil . ,ly:text-interface::print)
	(meta . ((class . Item)
		 (interfaces . (note-name-interface
				text-interface
				font-interface))))))


    (OctavateEight
     . (
	(self-alignment-X . ,CENTER)
	(break-visibility . ,begin-of-line-visible)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::centered-on-x-parent)))))	
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(stencil . ,ly:text-interface::print)
	(font-shape . italic)
	(staff-padding . 0.2)
	(font-size . -4)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				self-alignment-interface
				side-position-interface
				font-interface))))))

    (OttavaBracket
     . (
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(stencil . ,ly:ottava-bracket::print)
	(font-shape . italic)
	(shorten-pair . (0.0 . -0.6))
	(staff-padding . 1.0)
	(padding . 0.5)
	(minimum-length . 1.0)
	(style . dashed-line)
	(dash-fraction . 0.3)
	(edge-height . (0 . 1.2))
	(direction . ,UP)
	(outside-staff-priority . 400)
	(meta . ((class . Spanner)
		 (interfaces . (ottava-bracket-interface
				horizontal-bracket-interface
				line-interface
				side-position-interface
				font-interface
				text-interface))))))


    (PaperColumn
     . (
	(axes . (,X))
	(allow-loose-spacing . #t)
	(before-line-breaking . ,ly:paper-column::before-line-breaking)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	;; (stencil . ,ly:paper-column::print)
	(X-extent . ,ly:axis-group-interface::width)
	
	;; debugging
	;;		         (font-size . -6) (font-name . "sans") (Y-extent . #f)
	(meta . ((class . Paper_column)
		 (interfaces . (paper-column-interface
				separation-item-interface
				axis-group-interface
				spaceable-grob-interface
				font-interface))))))

    (ParenthesesItem
     . ((stencil . ,parentheses-item::print)
	(stencils . ,parentheses-item::calc-parenthesis-stencils)
	(font-size . -6)
	(padding . 0.2)
	(meta . ((class . Item)
		 (interfaces . (parentheses-interface font-interface))))))

    (HarmonicParenthesesItem
     . ((stencil . ,parentheses-item::print)
	(padding . 0)
	(stencils . ,parentheses-item::calc-angled-bracket-stencils)
	(meta . ((class . Item)
		 (interfaces . (parentheses-interface font-interface))))))
    
    (PhrasingSlur
     . ((details . ,default-slur-details)
	(control-points . ,ly:slur::calc-control-points)
	(direction . ,ly:slur::calc-direction)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(Y-extent . ,ly:slur::height)
	(stencil . ,ly:slur::print)		      
	(thickness . 1.1)
	(minimum-length . 1.5)
	(height-limit . 2.0)
	(ratio . 0.333)
	(cross-staff . ,ly:slur::calc-cross-staff)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    (PercentRepeat
     . (
	(springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
	(stencil . ,ly:multi-measure-rest::percent)
	(slope . 1.0)
	(thickness . 0.48)
	(dot-negative-kern . 0.75)
	(font-encoding . fetaMusic)
	(meta . ((class . Spanner)
		 (interfaces . (multi-measure-rest-interface
				font-interface
				percent-repeat-interface))))))

    (PercentRepeatCounter
     . (
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-centered-on-y-parent))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(self-alignment-X . ,CENTER)
	(direction . ,UP)
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
	(stencil . ,ly:piano-pedal-bracket::print)
	(style . line)
	(bound-padding . 1.0)
	(direction . ,DOWN)
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
	(stencil . ,ly:text-interface::print)
	(X-offset . ,(ly:make-simple-closure
		      `(,+
			,(ly:make-simple-closure
			  (list ly:break-alignable-interface::self-align-callback))
			,(ly:make-simple-closure
			  (list ly:self-alignment-interface::x-aligned-on-self)))))
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(self-alignment-X . ,CENTER)
	(direction . ,UP)
	(non-musical . #t)
	(font-size . 2)
	(baseline-skip . 2)
	(break-visibility . ,end-of-line-invisible)
	(break-align-symbols . (staff-bar clef))
	(padding . 0.8)
	(outside-staff-priority . 1500)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				side-position-interface
				break-alignable-interface
				font-interface
				mark-interface
				self-alignment-interface))))))

    (RepeatSlash
     . (
	(stencil . ,ly:percent-repeat-item-interface::beat-slash)
	(thickness . 0.48)
	(slope . 1.7)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-grob-interface
				percent-repeat-interface
				percent-repeat-item-interface))))))

    (RepeatTie
     . (
	(stencil  . ,ly:tie::print)
	(control-points . ,ly:semi-tie::calc-control-points)
	(direction . ,ly:tie::calc-direction)
	(details . ((ratio . 0.333)
		    (height-limit . 1.0)))
	(thickness . 1.0)
	(head-direction . ,RIGHT)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-interface))))))

    (RepeatTieColumn
     . (
	(X-extent . #f)
	(Y-extent . #f)
	(direction . ,ly:tie::calc-direction)
	(head-direction . ,ly:semi-tie-column::calc-head-direction)
	
	(positioning-done . ,ly:semi-tie-column::calc-positioning-done)
	(meta . ((class . Item)
		 (interfaces . (semi-tie-column-interface))))))

    (Rest
     . (
	(stencil . ,ly:rest::print)
	(duration-log . ,stem::calc-duration-log)
	(X-extent . ,ly:rest::width)
	(Y-extent . ,ly:rest::height)
	(Y-offset . ,ly:rest::y-offset-callback)
	(minimum-distance . 0.25)
	(cross-staff . ,ly:rest::calc-cross-staff)
	(meta . ((class . Item)
		 (interfaces . (font-interface
				rhythmic-head-interface
				rhythmic-grob-interface
				staff-symbol-referencer-interface
				rest-interface))))))

    (RestCollision
     . (
	(minimum-distance . 0.75)
	(positioning-done . ,ly:rest-collision::calc-positioning-done)
	(meta . ((class . Item)
		 (interfaces . (rest-collision-interface))))))


    (Script
     . (
	;; don't set direction here: it breaks staccato.

	;; padding set in script definitions.
	(staff-padding . 0.25)
	(X-offset . ,script-interface::calc-x-offset)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)

	(stencil . ,ly:script-interface::print)
	(direction . ,ly:script-interface::calc-direction)
	(positioning-done . ,ly:script-interface::calc-positioning-done)
	(font-encoding . fetaMusic)
	(cross-staff . ,ly:script-interface::calc-cross-staff)
	(meta . ((class . Item)
		 (interfaces . (script-interface
				side-position-interface
				font-interface))))))

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
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(horizontal-skylines . ,ly:separation-item::calc-skylines)
	(stencil . ,ly:separation-item::print)
	(meta . ((class . Item)
		 (interfaces . (separation-item-interface))))))

    (Slur
     . (
	(details . ,default-slur-details)
	(control-points . ,ly:slur::calc-control-points)
	(direction . ,ly:slur::calc-direction)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(Y-extent . ,ly:slur::height)
	(stencil . ,ly:slur::print)
	(thickness . 1.2)
	(line-thickness . 0.8)
	(minimum-length . 1.5)
	(height-limit . 2.0)
	(ratio . 0.25)
	(avoid-slur . inside)
	(cross-staff . ,ly:slur::calc-cross-staff)
	(meta . ((class . Spanner)
		 (interfaces . (slur-interface))))))

    (SostenutoPedal
     . (
	(stencil . ,ly:text-interface::print)
	(direction . ,RIGHT)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(padding . 0.0) ;; padding relative to SostenutoPedalLineSpanner
	(font-shape . italic)
	(self-alignment-X . ,CENTER)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				self-alignment-interface
				piano-pedal-script-interface
				font-interface))))))

    (SostenutoPedalLineSpanner
     . (
	(axes . (,Y))
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(outside-staff-priority . 1000)
	(side-axis . ,Y)
	(staff-padding . 1.0)
	(padding . 1.2)
	(minimum-space . 1.0)
	(direction . ,DOWN)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))

    (SpacingSpanner
     . (
	(springs-and-rods . ,ly:spacing-spanner::set-springs)
	(common-shortest-duration . ,ly:spacing-spanner::calc-common-shortest-duration)
	(average-spacing-wishes . #t)
	(shortest-duration-space . 2.0)
	(spacing-increment . 1.2)
	
	(base-shortest-duration . ,(ly:make-moment 3 16))
	(meta . ((class . Spanner)
		 (interfaces . (spacing-options-interface
				spacing-spanner-interface))))))

    (SpanBar
     . (
	(break-align-symbol . staff-bar)
	(Y-extent . ,ly:axis-group-interface::height)
	(cross-staff . #t)
	(layer . 0)
	(non-musical . #t)
	(stencil . ,ly:span-bar::print)
	(bar-size . ,ly:span-bar::calc-bar-size)
	(bar-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:span-bar::width)
	(glyph-name . ,ly:span-bar::calc-glyph-name)
	(before-line-breaking . ,ly:span-bar::before-line-breaking)
	(allow-span-bar . #t)

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
	(stencil . ,ly:text-interface::print)
	(font-series . bold)
	(padding . 1.0)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(side-axis . ,X)
	(direction . ,LEFT)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				stanza-number-interface
				text-interface
				font-interface))))))

    (StaffSpacing
     . (
	(non-musical . #t)
	(stem-spacing-correction . 0.4)
	(meta . ((class . Item)
		 (interfaces . (spacing-interface
				staff-spacing-interface))))))
   
    (StaffSymbol
     . (
	(Y-extent . ,ly:staff-symbol::height)
	(stencil . ,ly:staff-symbol::print)
	(line-count . 5)
	(ledger-line-thickness . (1.0 . 0.1))
	(layer . 0)
	(meta . ((class . Spanner)
		 (interfaces . (staff-symbol-interface))))))

    (Stem
     . (
	(direction . ,ly:stem::calc-direction)
	(duration-log . ,stem::calc-duration-log)
	(default-direction . ,ly:stem::calc-default-direction)
	(stem-end-position . ,ly:stem::calc-stem-end-position)

	(neutral-direction . ,DOWN)
	(stem-info . ,ly:stem::calc-stem-info)
	(positioning-done . ,ly:stem::calc-positioning-done)
	(stencil . ,ly:stem::print)
	(X-extent . ,ly:stem::width)
	(Y-extent . ,ly:stem::height)
	(length . ,ly:stem::calc-length)
	(thickness . 1.3)
	(cross-staff . ,ly:stem::calc-cross-staff)
	(flag . ,ly:stem::calc-flag)
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

	;; We use the normal minima as minimum for the ideal lengths,
	;; and the extreme minima as abolute minimum length.

	(X-offset . ,ly:stem::offset-callback)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(meta . ((class . Item)
		 (interfaces . (stem-interface
				font-interface))))))

    (StemTremolo
     . (
	(Y-extent . ,ly:stem-tremolo::height)
	(X-extent . ,ly:stem-tremolo::width)
	(stencil . ,ly:stem-tremolo::print)
	(slope . ,ly:stem-tremolo::calc-slope)
	(beam-width . ,ly:stem-tremolo::calc-width) ; staff-space
        (style . ,ly:stem-tremolo::calc-style)
	(beam-thickness . 0.48) ; staff-space
	(meta . ((class . Item)
		 (interfaces . (stem-tremolo-interface))))))

    (StringNumber
     . (
	(stencil . ,print-circled-text-callback)
	(text . ,string-number::calc-text)
	(padding . 0.5)
	(staff-padding . 0.5)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(script-priority . 100)
	(avoid-slur . around)
	(font-encoding . fetaNumber)
	(font-size . -5) 		; don't overlap when next to heads.
	(meta . ((class . Item)
		 (interfaces . (string-number-interface
				font-interface
				text-script-interface
				text-interface
				side-position-interface
				self-alignment-interface))))))
    
    (StrokeFinger
     . (
	(stencil . ,ly:text-interface::print)
	(text . ,stroke-finger::calc-text)
	(digit-names . #("p" "i" "m" "a" "x"))
	(padding . 0.5)
	(staff-padding . 0.5)
	(self-alignment-X . ,CENTER)
	(self-alignment-Y . ,CENTER)
	(script-priority . 100)
	(font-shape . italic)
	(font-size . -4) 		; don't overlap when next to heads.
	(meta . ((class . Item)
		 (interfaces . (stroke-finger-interface
				font-interface
				text-script-interface
				text-interface
				side-position-interface
				self-alignment-interface))))))    

    (SustainPedal
     . (
	(extra-spacing-width . (+inf.0 . -inf.0))
	(stencil . ,ly:sustain-pedal::print)
	(self-alignment-X . ,CENTER)
	(direction . ,RIGHT)
	(padding . 0.0)  ;; padding relative to SustainPedalLineSpanner
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (piano-pedal-interface
				text-interface
				self-alignment-interface
				piano-pedal-script-interface
				font-interface))))))

    (SustainPedalLineSpanner
     . (
	(axes . (,Y))
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(outside-staff-priority . 1000)
	(side-axis . ,Y)
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . ,DOWN)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))

    (System
     . (
	(axes . (,X ,Y))
	(X-extent . ,ly:axis-group-interface::width)
	(Y-extent . ,ly:axis-group-interface::height)
	(vertical-skylines . ,ly:axis-group-interface::calc-skylines)
	(max-stretch . ,ly:axis-group-interface::calc-max-stretch)
	(meta . ((class . System)
		 (interfaces . (system-interface
				axis-group-interface))))))

    (SystemStartBrace
     . (
	(style . brace)
	(padding . 0.3)
	(stencil . ,ly:system-start-delimiter::print)
	(collapse-height . 5.0)
	(X-offset . ,ly:side-position-interface::x-aligned-side)
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
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(stencil . ,ly:system-start-delimiter::print)
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
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(stencil . ,ly:system-start-delimiter::print)
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

	;; ugh--hardcoded. 
	(padding . -0.1)  ;; bar must cover rounded ending of staff line.
	(X-offset . ,ly:side-position-interface::x-aligned-side)
	(direction . ,LEFT)
	(style . bar-line)
	(collapse-height . 5.0)
	(thickness . 1.6)
	(stencil . ,ly:system-start-delimiter::print)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				system-start-delimiter-interface))))))


    (TabNoteHead
     . (
	(stencil . ,ly:text-interface::print)
	(duration-log . ,note-head::calc-duration-log)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(direction . ,CENTER)
	(font-size . -2)
	(stem-attachment . (0.0 . 1.35))
	(font-series . bold)
	(meta . ((class . Item)
		 (interfaces  . (rhythmic-head-interface
				 font-interface
				 rhythmic-grob-interface
				 note-head-interface
				 staff-symbol-referencer-interface
				 text-interface))))))

    (TextScript
     . (
	(extra-spacing-width . (+inf.0 . -inf.0))
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(direction . ,DOWN)

	;; sync with Fingering ?
	(padding . 0.5)
	(staff-padding . 0.5)

	(stencil . ,ly:text-interface::print)
	(direction . ,ly:script-interface::calc-direction)
	(positioning-done . ,ly:script-interface::calc-positioning-done)

	(outside-staff-priority . 450)
	(avoid-slur . around)
	(slur-padding . 0.5)
	(script-priority . 200)
	(cross-staff . ,ly:script-interface::calc-cross-staff)
	;; todo: add X self alignment?
	(meta . ((class . Item)
		 (interfaces . (text-script-interface
				text-interface
				instrument-specific-markup-interface
				side-position-interface
				self-alignment-interface
				font-interface))))))

    (TextSpanner
     . (
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(font-shape . italic)
	(style . dashed-line)
	(staff-padding . 0.8)
	(dash-fraction . 0.2)
	(dash-period . 3.0)
	(side-axis . ,Y)
	(direction . ,UP)
	(outside-staff-priority . 350)

	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(bound-details . ((left . ((Y . 0)
				   (padding . 0.25)
				   (attach-dir . ,LEFT)
				   ))
			  (right . ((Y . 0)
				    (padding . 0.25)
				    ))
			  ))
	(stencil . ,ly:line-spanner::print)

	(meta . ((class . Spanner)
		 (interfaces . (line-spanner-interface
				line-interface
				side-position-interface
				font-interface))))))

    (Tie
     . (
	(control-points . ,ly:tie::calc-control-points)
	(springs-and-rods . ,ly:spanner::set-spacing-rods)
	(avoid-slur . inside)
	(direction . ,ly:tie::calc-direction)
	(neutral-direction . ,UP)
	(stencil . ,ly:tie::print)
	(font-size . -6)
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
	
	(thickness . 1.2)
	(line-thickness . 0.8)
	(meta . ((class . Spanner)
		 (interfaces . (tie-interface))))))

    (TieColumn
     . (
	(positioning-done . ,ly:tie-column::calc-positioning-done)
	(before-line-breaking . ,ly:tie-column::before-line-breaking)
	(X-extent . #f)
	(Y-extent . #f)
	(meta . ((class . Spanner)
		 (interfaces . (tie-column-interface))))))

    (TimeSignature
     . (
	(stencil . ,ly:time-signature::print)
	(break-align-symbol . time-signature)
	(break-align-anchor
	 . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
	(break-visibility . ,all-visible)
	(avoid-slur . inside)
	(extra-spacing-height . (-1.0 . 1.0))
	(space-alist . (
			(first-note . (fixed-space . 2.0))
			(right-edge . (extra-space . 0.5))
			(staff-bar . (minimum-space . 2.0))))
	(non-musical . #t)
	(style . C)
	(meta . ((class . Item)
		 (interfaces . (time-signature-interface
				break-aligned-interface
				font-interface))))))

    (TrillSpanner
     . (
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)

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
	
	(stencil . ,ly:line-spanner::print)

	(style . trill)
	(staff-padding . 1.0)
	(padding . 0.5)
	(direction . ,UP)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(side-axis . ,Y)
	(outside-staff-priority . 50)
	(meta . ((class . Spanner)
		 (interfaces . (line-spanner-interface
				line-interface
				trill-spanner-interface
				side-position-interface
				font-interface))))))

    (TrillPitchAccidental
     . ((X-offset . ,ly:side-position-interface::x-aligned-side)
	(padding . 0.2)
	(direction . ,LEFT)
	(font-size . -4)
	(side-axis . ,X)
	(stencil . ,ly:accidental-interface::print)
	(Y-extent . ,ly:accidental-interface::height)
	(glyph-name-alist . ,standard-alteration-glyph-name-alist)
	(meta . ((class . Item)
		 (interfaces . (trill-pitch-accidental-interface
				accidental-interface
				side-position-interface
				font-interface))))))

    (TrillPitchGroup
     . ((X-offset . ,ly:side-position-interface::x-aligned-side)
	(axes . (,X))
	(font-size . -4)
	(stencil . ,parenthesize-elements)
	(stencils . ,parentheses-item::calc-parenthesis-stencils)
	(direction . ,RIGHT)
	(side-axis . ,X)
	(padding . 0.3)
	(meta . ((class . Item)
		 (interfaces . (side-position-interface
				parentheses-interface
				note-head-interface
				font-interface
				axis-group-interface))))))

    (TrillPitchHead
     . (
	(stencil . ,ly:note-head::print)
	(duration-log . 2)
	(Y-offset . ,ly:staff-symbol-referencer::callback)
	(font-size . -4)
	(meta . ((class . Item)
		 (interfaces . (rhythmic-head-interface
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
	(full-length-to-extent . #t)
	(direction  . ,ly:tuplet-bracket::calc-direction)
	(positions . ,ly:tuplet-bracket::calc-positions)
	(connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
	(control-points . ,ly:tuplet-bracket::calc-control-points)
	(stencil . ,ly:tuplet-bracket::print)
	(cross-staff . ,ly:tuplet-bracket::calc-cross-staff)
	
	(meta . ((class . Spanner)
		 (interfaces . (line-interface
				tuplet-bracket-interface))))))

    (TupletNumber
     . (
	(stencil . ,ly:tuplet-number::print)
	(text . ,tuplet-number::calc-denominator-text)
	(font-shape . italic)
	(font-size . -2)
	(avoid-slur . inside)
	(cross-staff . ,ly:tuplet-number::calc-cross-staff)
	(meta . ((class . Spanner)
		 (interfaces . (text-interface tuplet-number-interface
				font-interface))))))

    
    (UnaCordaPedal
     . (
	(stencil . ,ly:text-interface::print)
	(font-shape . italic)
	(extra-spacing-width . (+inf.0 . -inf.0))
	(self-alignment-X . ,CENTER)
	(direction . ,RIGHT)
	(padding . 0.0)  ;; padding relative to UnaCordaPedalLineSpanner
	(X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
	(meta . ((class . Item)
		 (interfaces . (text-interface
				piano-pedal-script-interface
				self-alignment-interface
				font-interface))))))

    (UnaCordaPedalLineSpanner
     . (
	(axes . (,Y))
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(outside-staff-priority . 1000)
	(side-axis . ,Y)
	(padding . 1.2)
	(staff-padding . 1.2)
	(minimum-space . 1.0)
	(direction . ,DOWN)
	(meta . ((class . Spanner)
		 (interfaces . (piano-pedal-interface
				axis-group-interface
				side-position-interface))))))


    (VaticanaLigature
     . (
	(thickness . 0.6)
	(flexa-width . 2.0)
	(stencil . ,ly:vaticana-ligature::print)
	(meta . ((class . Spanner)
		 (interfaces . (vaticana-ligature-interface
				font-interface))))))

    (VerticalAlignment
     . (
	(axes . (,Y))
	(positioning-done . ,ly:align-interface::calc-positioning-done)
	(after-line-breaking . ,ly:align-interface::stretch-after-break)
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(stacking-dir . -1)
	(padding . 0.5)
	(vertical-skylines . ,ly:axis-group-interface::combine-skylines)
	(max-stretch . 0)
	(meta . ((class . Spanner)
		 (object-callbacks . ((Y-common . ,ly:axis-group-interface::calc-y-common)))
		 (interfaces . (align-interface
				axis-group-interface))))))

    (VerticalAxisGroup
     . (
	(axes . (,Y))
	(Y-offset . ,ly:hara-kiri-group-spanner::force-hara-kiri-callback)
	(Y-extent . ,ly:hara-kiri-group-spanner::y-extent)
	(X-extent . ,ly:axis-group-interface::width)
	(vertical-skylines . ,ly:hara-kiri-group-spanner::calc-skylines)
	(max-stretch . ,ly:axis-group-interface::calc-max-stretch)
	(stencil . ,ly:axis-group-interface::print)
	(adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
	(meta . ((class . Spanner)
		 (object-callbacks . ((X-common . ,ly:axis-group-interface::calc-x-common)))
		 (interfaces . (axis-group-interface
				hara-kiri-group-spanner-interface
				vertically-spaceable-interface))))))

    (VoltaBracket
     . (
	(stencil . ,ly:volta-bracket-interface::print)
	(font-encoding . fetaNumber)
	(thickness . 1.6)  ;;  line-thickness
	(edge-height . (2.0 . 2.0)) ;; staff-space;
	(font-size . -4)
	(word-space . 0.6)
	(direction . ,UP)
	(meta . ((class . Spanner)
		 (interfaces . (volta-bracket-interface
				horizontal-bracket-interface				
				line-interface
				text-interface
				side-position-interface
				font-interface))))))

    (VoltaBracketSpanner
     . (	
	(axes . (,Y))
	(side-axis . ,Y)
	(direction . ,UP)
	(padding . 1)
	(Y-offset . ,ly:side-position-interface::y-aligned-side)
	(outside-staff-priority . 100)
	(Y-extent . ,ly:axis-group-interface::height)
	(X-extent . ,ly:axis-group-interface::width)
	(no-alignment . ,#t)
	(meta . ((class . Spanner)
		 (interfaces . (side-position-interface
				axis-group-interface))))))

    (VoiceFollower
     . (
	(style . line)
	(gap . 0.5)
	(non-musical . #t)
	(X-extent . #f)
	(Y-extent . #f)
	(bound-details . ((right . ((attach-dir .  ,CENTER)
				    (padding . 1.5)
				      ))
			  (left . ((attach-dir .  ,CENTER)
				   (padding . 1.5)
				      ))
			  ))
	(stencil . ,ly:line-spanner::print)
	(left-bound-info . ,ly:line-spanner::calc-left-bound-info)
	(right-bound-info . ,ly:line-spanner::calc-right-bound-info)
	(meta . ((class . Spanner)
		 (interfaces . (line-spanner-interface
				line-interface))))))

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
    (,ly:stem::height . ,ly:stem::pure-height)
    (,ly:rest::height . ,ly:rest::pure-height)
    (,ly:grob::stencil-height . ,pure-stencil-height)
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
