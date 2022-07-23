;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

;;;; distances are given in line-thickness (thicknesses) and
;;;; staff-space (distances)

;;;; WARNING: the meta field should be the last one.

;;;; WARNING: Don't use anonymous functions for initialization.  They
;;;; won't have a useful representation in the Internals Reference.
;;;; They will also make the doc build unreproducible because Guile >=
;;;; 2 includes raw memory addresses in a function's displayed
;;;; representation.

;; TODO: junk the meta field in favor of something more compact?


(define all-grob-descriptions-data
  `(
    (Accidental
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (avoid-slur . inside)
        (glyph-name . ,accidental-interface::calc-glyph-name)
        (extra-spacing-width . (-0.2 . 0.0))
        (stencil . ,ly:accidental-interface::print)
        (horizontal-skylines . ,(ly:make-unpure-pure-container ly:accidental-interface::horizontal-skylines))
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                accidental-switch-interface
                                inline-accidental-interface
                                font-interface))
                 (description . "An accidental.  Horizontal
padding and configuration between accidentals is controlled
by the @iref{AccidentalPlacement} grob.")))))

    (AccidentalCautionary
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (avoid-slur . inside)
        (parenthesized . #t)
        (extra-spacing-width . (-0.2 . 0.0))
        (glyph-name . ,accidental-interface::calc-glyph-name)
        (stencil . ,ly:accidental-interface::print)
        (horizontal-skylines . ,(ly:make-unpure-pure-container ly:accidental-interface::horizontal-skylines))
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                accidental-switch-interface
                                inline-accidental-interface
                                font-interface))
                 (description . "A cautionary accidental, normally
enclosed in parentheses.")))))

    (AccidentalPlacement
     . (
        (direction .  ,LEFT)
        (positioning-done . ,ly:accidental-placement::calc-positioning-done)

        ;; this is quite small, but it is very ugly to have
        ;; accs closer to the previous note than to the next one.
        (right-padding . 0.15)

        ;; for horizontally stacked scripts.
        (script-priority .  -100)

        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Item)
                 (interfaces . (accidental-placement-interface))
                 (description . "In groups of @iref{Accidental}
grobs, this auxiliary grob controls their horizontal padding and
configuration (which ones are placed more to left or to the
right).")))))

    (AccidentalSuggestion
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (direction . ,UP)
        (font-size . -2)
        (glyph-name . ,accidental-interface::calc-glyph-name)
        (outside-staff-priority . 0)
        (parent-alignment-X . ,CENTER)
        (script-priority . 0)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (staff-padding . 0.25)
        (stencil . ,ly:accidental-interface::print)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,accidental-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                accidental-suggestion-interface
                                accidental-switch-interface
                                font-interface
                                outside-staff-interface
                                script-interface
                                self-alignment-interface
                                side-position-interface))
                 (description . "An annotational accidental as used
in @dfn{musica ficta}.  Normally positioned above a note.")))))

    (Ambitus
     . (
        (axes . (,X ,Y))
        (break-align-symbol . ambitus)
        (break-visibility . ,begin-of-line-visible)
        (non-musical . #t)
        (space-alist . (
                        (cue-end-clef . (extra-space . 0.5))
                        (clef . (extra-space . 1.15))
                        (cue-clef . (extra-space . 0.5))
                        (key-signature . (extra-space . 1.15))
                        (signum-repetitionis . (extra-space . 1.15))
                        (staff-bar . (extra-space . 1.15))
                        (time-signature . (extra-space . 1.15))
                        (right-edge . (extra-space . 0.5))
                        (first-note . (extra-space . 1.15))))
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (ambitus-interface
                                axis-group-interface
                                break-aligned-interface))
                 (description . "An ambitus, giving the range of
pitches of a voice or instrument.  It aligns
@iref{AmbitusAccidental}, @iref{AmbitusLine},
and @iref{AmbitusNoteHead} horizontally and defines the
horizontal spacing from the ambitus to other items.")))))

    (AmbitusAccidental
     . (
        (glyph-name . ,accidental-interface::calc-glyph-name)
        (stencil . ,ly:accidental-interface::print)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                accidental-switch-interface
                                break-aligned-interface
                                font-interface))
                 (description . "An accidental in an
@iref{Ambitus}.")))))

    (AmbitusLine
     . (
        (gap . ,ambitus-line::calc-gap)
        (length-fraction . 0.7)
        (maximum-gap . 0.45)
        (stencil . ,ambitus::print)
        (thickness . 2)
        (X-offset . ,ly:self-alignment-interface::centered-on-x-parent)
        (meta . ((class . Item)
                 (interfaces . (ambitus-interface
                                font-interface))
                 (description . "The vertical line in an
@iref{Ambitus}.")))))

    (AmbitusNoteHead
     . (
        (duration-log . 2)
        (glyph-name . ,note-head::calc-glyph-name)
        (ignore-ambitus . #t)
        (stencil . ,ly:note-head::print)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (ambitus-interface
                                font-interface
                                ledgered-interface
                                note-head-interface
                                staff-symbol-referencer-interface))
                 (description . "A note head in an
@iref{Ambitus}.")))))

    (Arpeggio
     . (
        (cross-staff . ,ly:arpeggio::calc-cross-staff)
        (direction . ,LEFT)
        (line-thickness . 1)
        (padding . 0.5)
        (positions . ,ly:arpeggio::calc-positions)
        (protrusion . 0.4)
        (script-priority . 0)
        (side-axis . ,X)
        (staff-position . 0.0)
        (stencil . ,ly:arpeggio::print)
        (thickness . 1)
        (X-extent . ,ly:arpeggio::width)
        (Y-extent . ,(grob::unpure-Y-extent-from-stencil ly:arpeggio::pure-height))
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Item)
                 (interfaces . (arpeggio-interface
                                font-interface
                                side-position-interface
                                staff-symbol-referencer-interface))
                 (description . "An arpeggio line (normally a
vertical wiggle).")))))


    (BalloonText
     . (
        (after-line-breaking . ,ly:balloon-interface::remove-irrelevant-spanner)
        (annotation-balloon . #t)
        (annotation-line . #t)
        (break-visibility . ,(sticky-grob-interface::inherit-property
                              'break-visibility))
        (cross-staff . ,(sticky-grob-interface::inherit-property
                         'cross-staff))
        (extra-spacing-width . (+inf.0 . -inf.0))
        (spanner-placement . ,LEFT)
        (stencil . ,ly:balloon-interface::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (thickness . 1.0)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (X-extent . ,ly:balloon-interface::width)
        (Y-extent . ,balloon::height)
        (meta . ((classes . (Item Spanner))
                 (interfaces . (accidental-switch-interface
                                balloon-interface
                                font-interface
                                sticky-grob-interface
                                text-interface))
                 (description . "A balloon text with a pointing
line to visually mark and annotate another grob.")))))

    (BarLine
     . (
        (allow-span-bar . #t)
        (bar-extent . ,ly:bar-line::calc-bar-extent)
        (break-align-anchor . ,ly:bar-line::calc-anchor)
        (break-align-symbol . staff-bar)
        (break-visibility . ,bar-line::calc-break-visibility)
        (extra-spacing-height . ,pure-from-neighbor-interface::account-for-span-bar)
        (gap . 0.4)
        (glyph . "|")
        (glyph-left . ,(grob::relay-other-property 'glyph))
        (glyph-name . ,bar-line::calc-glyph-name)
        (glyph-right . #f)

        ;;
        ;; Ross. page 151 lists other values, we opt for a leaner look
        ;;
        ;; TODO:
        ;; kern should scale with line-thickness too.
        (kern . 3.0)
        (segno-kern . 3.0)
        (hair-thickness . 1.9)
        ;; For chord squares to determine the ending of inner lines.
        (horizontal-skylines . ,grob::always-horizontal-skylines-from-stencil)
        (thick-thickness . 6.0)

        (layer . 0)
        (non-musical . #t)
        (rounded . #f)
        (space-alist . (
                        (ambitus . (extra-space . 1.0))
                        (time-signature . (extra-space . 0.75))
                        (custos . (minimum-space . 2.0))
                        (clef . (extra-space . 1.0))
                        (key-signature . (extra-space . 1.0))
                        (key-cancellation . (extra-space . 1.0))
                        (first-note . (fixed-space . 1.3))
                        (next-note . (semi-fixed-space . 0.9))
                        (right-edge . (extra-space . 0.0))))
        (stencil . ,ly:bar-line::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (bar-line-interface
                                break-aligned-interface
                                font-interface
                                pure-from-neighbor-interface))
                 (description . "A bar line.")))))

    (BarNumber
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        ;; want the bar number before the clef at line start.
        (break-align-symbols . (left-edge staff-bar))

        (break-visibility . ,begin-of-line-visible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-family . roman)
        (font-size . -2)
        (non-musical . #t)
        ;; w/o padding, bars numbers are not positioned over the staff as
        ;; they are slightly to the left. so we add just a bit.
        (horizon-padding . 0.05)
        (outside-staff-priority . 100)
        (padding . 1.0)
        (self-alignment-X . ,(break-alignment-list LEFT LEFT RIGHT))
        (side-axis . ,Y)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta .
              ((class . Item)
               (interfaces . (bar-number-interface
                              break-alignable-interface
                              font-interface
                              outside-staff-interface
                              self-alignment-interface
                              side-position-interface
                              text-interface))
               (description . "An ordinary bar number.  Centered
bar numbers are managed separately with @iref{CenteredBarNumber}
grobs.")))))

    (BassFigure
     . (
        (font-features . ("tnum" "cv47" "ss01"))
        (stencil . ,ly:text-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                bass-figure-interface
                                font-interface
                                rhythmic-grob-interface
                                text-interface))
                 (description . "A number in figured bass.  It can contain an
alteration as well.")))))

    (BassFigureAlignment
     . (
        (axes . (,Y))
        (padding . -inf.0)
        (positioning-done . ,ly:align-interface::align-to-minimum-distances)
        (stacking-dir . ,DOWN)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (align-interface
                                axis-group-interface
                                bass-figure-alignment-interface))
                 (description . "An auxiliary grob to stack
several @iref{BassFigureLine} grobs vertically.")))))

    (BassFigureAlignmentPositioning
     . (
        (axes . (,Y))
        (direction . ,UP)
        (padding . 0.5)
        (side-axis . ,Y)
        (add-stem-support . #t)
        (staff-padding . 1.0)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "If figured bass is used in the
@iref{Staff} context, this auxiliary grob groups all of the
figured bass notation and computes an offset from the staff via
side-positioning.")))))

    (BassFigureBracket
     . (
        (edge-height . (0.2 . 0.2))
        (stencil . ,ly:enclosing-bracket::print)
        (X-extent . ,ly:enclosing-bracket::width)
        (meta . ((class . Item)
                 (interfaces . (enclosing-bracket-interface))
                 (description . "Brackets around a figured bass
(or elements of it).")))))

    (BassFigureContinuation
     . (
        (stencil . ,ly:figured-bass-continuation::print)
        (Y-offset . ,ly:figured-bass-continuation::center-on-figures)
        (meta . ((class . Spanner)
                 (interfaces . (figured-bass-continuation-interface))
                 (description . "A horizontal line to indicate
that a number of a previous figured bass is continued in the
current figured bass.")))))

    (BassFigureLine
     . (
        (adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
        (axes . (,Y))
        (staff-staff-spacing . ((minimum-distance . 1.5)
                                (padding . 0.1)))
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-axis-group-interface))
                 (description . "An auxiliary grob providing a
baseline for bass figures that should be aligned
vertically.")))))

    (Beam
     . (
        ;; todo: clean this up a bit: the list is getting
        ;; rather long.
        (auto-knee-gap . 5.5)
        (beam-segments . ,ly:beam::calc-beam-segments)
        (beam-thickness . 0.48) ; in staff-space

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
        (collision-interfaces . (beam-interface
                                 clef-interface
                                 clef-modifier-interface
                                 flag-interface
                                 inline-accidental-interface
                                 key-signature-interface
                                 note-head-interface
                                 stem-interface
                                 time-signature-interface))
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
           (collision-penalty . 500)
           (collision-padding . 0.35)
           (round-to-zero-slope . 0.02)))
        (direction . ,ly:beam::calc-direction)

        (normalized-endpoints . ,ly:spanner::calc-normalized-endpoints)
        ;; only for debugging.
        (font-family . roman)

        (gap . 0.8)
        (neutral-direction . ,DOWN)
        (positions . ,beam::place-broken-parts-individually)
        (X-positions . ,ly:beam::calc-x-positions)
        (transparent . ,(grob::inherit-parent-property
                         X 'transparent))

        ;; this is a hack to set stem lengths, if positions is set.
        (quantized-positions . ,ly:beam::set-stem-lengths)

        (shorten . ,ly:beam::calc-stem-shorten)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (stencil . ,ly:beam::print)

        (meta . ((class . Spanner)
                 (object-callbacks . ((normal-stems . ,ly:beam::calc-normal-stems)))
                 (interfaces . (beam-interface
                                staff-symbol-referencer-interface
                                unbreakable-spanner-interface))
                 (description . "A beam.")))))

    (BendAfter
     . (
        (minimum-length . 0.5)
        (stencil . ,bend::print)
        (thickness . 2.0)
        (meta . ((class . Spanner)
                 (interfaces . (bend-after-interface))
                 (description . "A grob for displaying
@dfn{falls} and @dfn{doits}.")))))

    (BendSpanner
     . (
        (avoid-slur . ignore)
        (baseline-skip . 3)
        (before-line-breaking . ,bend::target-cautionary)
        (cross-staff . #f)
        (details . ((arrow-stencil . ,bend::arrow-head-stencil)
                    ;; bend-arrow-curvature-factor, usually between 0 and 1,
                    ;; determines the horizontal part of a bend arrow as
                    ;; a percentage of the total horizontal extent
                    (curvature-factor . 0.35)
                    (bend-arrowhead-height . 1.25)
                    (bend-arrowhead-width . 0.8)
                    (bend-amount-strings . ((quarter . "¼")
                                            (half . "½")
                                            (three-quarter . "¾")
                                            (full . #f)))
                    (curve-x-padding-line-end . 0.5)
                    (curve-y-padding-line-end . 1)
                    ;; (dashed-line-settings . (on off phase))
                    (dashed-line-settings . (0.4 0.4 0))
                    ;; A vector of 3 booleans:
                    ;; #(end-of-line unbroken begin-of-line)
                    (head-text-break-visibility . #(#f #t #t))
                    (horizontal-left-padding . 0.1)
                    (successive-level . 1)
                    (target-visibility . #f)
                    (vertical-padding . 0.2)
                    (y-distance-from-tabstaff-to-arrow-tip . 2.75)))
        (direction . ,UP)
        (font-size . -2)
        (font-shape . italic)
        (font-encoding . latin1)
        (padding . 0.15)
        (side-axis . ,Y)
        (spanner-id . "")
        (stencil . ,bend-spanner::print)
        (style . ()) ;; hold, pre-bend, pre-bend-hold
        (text . #f)
        (thickness . 1)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (word-space . 0.6)
        (Y-offset . 0)
        (meta . ((class . Spanner)
                 (interfaces . (bend-interface
                                font-interface
                                line-spanner-interface
                                outside-staff-interface
                                text-interface
                                text-script-interface))
                 (description . "A string bending as used in
tablature notation.")))))

    (BreakAlignGroup
     . (
        (axes . (,X))
        (break-align-anchor . ,ly:break-aligned-interface::calc-average-anchor)
        (break-align-anchor-alignment . ,ly:break-aligned-interface::calc-joint-anchor-alignment)
        (break-visibility . ,ly:break-aligned-interface::calc-break-visibility)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Item)
                 (interfaces . (axis-group-interface
                                break-aligned-interface))
                 (description . "An auxiliary grob to group
several breakable items of the same type (clefs, time signatures,
etc.@:) across staves so that they will be aligned horizontally.
See also @iref{BreakAlignment}.")))))

    (BreakAlignment
     . (
        (axes . (,X))
        (break-align-orders . ;; end of line
                            #((
                               left-edge
                               staff-ellipsis
                               cue-end-clef
                               ambitus
                               breathing-sign
                               signum-repetitionis
                               clef
                               cue-clef
                               staff-bar
                               key-cancellation
                               key-signature
                               time-signature
                               custos)

                              ;; unbroken
                              (
                               left-edge
                               staff-ellipsis
                               cue-end-clef
                               ambitus
                               breathing-sign
                               signum-repetitionis
                               clef
                               cue-clef
                               staff-bar
                               key-cancellation
                               key-signature
                               time-signature
                               custos)

                              ;; begin of line
                              (
                               left-edge
                               staff-ellipsis
                               ambitus
                               breathing-sign
                               signum-repetitionis
                               clef
                               key-cancellation
                               key-signature
                               time-signature
                               staff-bar
                               cue-clef
                               custos)))
        (non-musical . #t)
        (positioning-done . ,ly:break-alignment-interface::calc-positioning-done)
        (stacking-dir . 1)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Item)
                 (interfaces . (axis-group-interface
                                break-alignment-interface))
                 (description . "An auxiliary grob that manages
the horizontal ordering of @iref{BreakAlignGroup} grobs within a
@iref{NonMusicalPaperColumn} grob (for example, whether the
time signature follows or precedes a bar line).")))))

    (BreathingSign
     . (
        (break-align-symbol . breathing-sign)
        (break-visibility . ,begin-of-line-invisible)
        (direction . ,UP)
        (non-musical . #t)
        (space-alist . (
                        (ambitus . (extra-space . 2.0))
                        (custos . (minimum-space . 1.0))
                        (key-signature . (minimum-space . 1.5))
                        (time-signature . (minimum-space . 1.5))
                        (signum-repetitionis . (minimum-space . 1.5))
                        (staff-bar . (minimum-space . 1.5))
                        (clef . (minimum-space . 2.0))
                        (cue-clef . (minimum-space . 2.0))
                        (cue-end-clef . (minimum-space . 2.0))
                        (first-note . (fixed-space . 1.0)) ;huh?
                        (right-edge . (extra-space . 0.1))))
        (stencil . ,ly:text-interface::print)
        ;; Thickness applies to Gregorian divisiones.  This default is
        ;; the same as BarLine.hair-thickness to allow them to blend
        ;; into modern contexts.
        (thickness . 1.9)
        (Y-offset . ,(ly:make-unpure-pure-container ly:breathing-sign::offset-callback))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                breathing-sign-interface
                                font-interface
                                outside-staff-interface
                                text-interface))
                 (description . "A breathing sign.")))))


    (CaesuraScript
     ;; CaesuraScript is similar to Script for vertical stacking, but
     ;; is non-musical.
     . (
        (before-line-breaking . ,caesura-script-interface::before-line-breaking)
        (break-visibility . ,begin-of-line-invisible)
        (direction . ,ly:script-interface::calc-direction)
        (font-encoding . fetaMusic)
        (horizon-padding . 0.1) ; to avoid interleaving with accidentals
        (non-musical . #t)
        (positioning-done . ,ly:script-interface::calc-positioning-done)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)

        ;; padding set in script definitions.
        (slur-padding . 0.2)
        (staff-padding . 0.25)

        (stencil . ,ly:script-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-offset . ,script-interface::calc-x-offset)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (caesura-script-interface
                                font-interface
                                outside-staff-interface
                                script-interface
                                self-alignment-interface
                                side-position-interface))
                 (description . "A script for @code{\\caesura}, e.g.,
an outside-staff comma or a fermata over a bar line.")))))

    (CenteredBarNumber
     . (
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-family . roman)
        ;; This is intentionally bigger than BarNumber.
        (font-size . 0)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,centered-spanner-interface::calc-x-offset)
        (meta .
              ((class . Spanner)
               (interfaces . (bar-number-interface
                              centered-bar-number-interface
                              centered-spanner-interface
                              font-interface
                              text-interface))
               (description . "A centered bar number; see also
@iref{CenteredBarNumberLineSpanner}.  Ordinary bar numbers are
managed with @iref{BarNumber} grobs.")))))

    (CenteredBarNumberLineSpanner
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (axes . (,Y))
        (direction . ,UP)
        (outside-staff-priority . 1200)
        (padding . 4)
        (side-axis . ,Y)
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                bar-number-interface
                                centered-bar-number-line-spanner-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "An auxiliary grob providing a
vertical baseline to align @iref{CenteredBarNumber} grobs.")))))

    (ChordName
     . (
        (after-line-breaking . ,ly:chord-name::after-line-breaking)
        (font-family . sans)
        (font-size . 1.5)
        (stencil . ,ly:text-interface::print)
        (extra-spacing-height . (0.2 . -0.2))
        (extra-spacing-width . (-0.5 . 0.5))
        (word-space . 0.0)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                chord-name-interface
                                font-interface
                                outside-staff-interface
                                rhythmic-grob-interface
                                text-interface))
                 (description . "A stand-alone chord name.  For chord
names in chord grids, see @iref{GridChordName}.")))))

    ;; About chord square notation, see: Philippe Baudoin, "Jazz, mode d'emploi
    ;; / Petite encyclopédie des données techniques de base".
    (ChordSquare
     . ((measure-division-chord-placement-alist
         . ,default-measure-division-chord-placement-alist)
        (measure-division-lines-alist
         . ,default-measure-division-lines-alist)
        (stencil . ,chord-square::print)
        ;; The default extent-based skylines are fine: the stencil's
        ;; skylines are a rectangle anyway.
        (X-extent . ,chord-square::width)
        (Y-extent . ,(ly:make-unpure-pure-container chord-square::height))
        (meta . ((class . Spanner)
                 (interfaces . (chord-square-interface
                                line-interface))
                 (description . "In a chord grid, this grob represents one chord
square.  It helps place @iref{GridChordName} grobs, and draws lines to separate
them. Note that this grob only draws the diagonal lines in a square.  The borders
of the square are drawn by @iref{StaffSymbol} and @iref{BarLine}.")))))

    (Clef
     . (
        (avoid-slur . inside)
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-anchor-alignment . ,RIGHT)
        (break-align-symbol . clef)
        (break-visibility . ,begin-of-line-visible)
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-at-beginning-of-line)
        (glyph-name . ,ly:clef::calc-glyph-name)
        (non-musical . #t)
        (space-alist . ((cue-clef . (extra-space . 2.0))
                        (signum-repetitionis . (extra-space . 0.7))
                        (staff-bar . (extra-space . 0.7))
                        (ambitus . (extra-space . 1.15))
                        (key-cancellation . (minimum-space . 3.5))
                        (key-signature . (minimum-space . 3.5))
                        (time-signature . (minimum-space . 4.2))
                        (first-note . (minimum-fixed-space . 5.0))
                        (next-note . (extra-space . 1.0))
                        (right-edge . (extra-space . 0.5))))
        (stencil . ,ly:clef::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (break-aligned-interface
                                clef-interface
                                font-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))
                 (description . "A clef.  See also
@iref{ClefModifier}, @iref{CueClef}, and @iref{CueEndClef}.")))))

    (ClefModifier
     . (
        (break-visibility . ,(grob::inherit-parent-property
                              X 'break-visibility))
        (clef-alignments . ((G . (-0.2 . 0.1))
                            (F . (-0.3 . -0.2))
                            (C . (0 . 0))))
        (color . ,(grob::inherit-parent-property
                   X 'color))
        (font-shape . italic)
        (font-size . -4)
        (parent-alignment-X . ,ly:clef-modifier::calc-parent-alignment)
        (self-alignment-X . ,CENTER)
        (staff-padding . 0.7)
        (stencil . ,ly:text-interface::print)
        (transparent . ,(grob::inherit-parent-property
                         X 'transparent))
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (clef-modifier-interface
                                font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob that draws the clef
modifier (if present), in most cases the digit@tie{}8 below or
above the clef.  See also @iref{Clef}, @iref{CueClef}, and
@iref{CueEndClef}.")))))

    (ClusterSpanner
     . (
        (cross-staff . ,ly:cluster::calc-cross-staff)
        (minimum-length . 0.0)
        (padding . 0.25)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:cluster::print)
        (style . ramp)
        (meta . ((class . Spanner)
                 (interfaces . (cluster-interface))
                 (description . "A cluster spanner.  The envelope
shape within the spanner is given by @iref{ClusterSpannerBeacon}
grobs.")))))

    (ClusterSpannerBeacon
     . (
        (Y-extent . ,ly:cluster-beacon::height)
        (meta . ((class . Item)
                 (interfaces . (cluster-beacon-interface
                                rhythmic-grob-interface))
                 (description . "An auxiliary grob to specify the
minimum and maximum pitch of a @iref{ClusterSpanner} grob at a
given moment.")))))

    (CodaMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (staff-bar key-signature clef))
        (break-visibility . ,begin-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-size . 2)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-padding . 0.4)
        (outside-staff-priority . 1400) ; inside RehearsalMark
        (padding . 0.4)
        (self-alignment-X . ,break-alignable-interface::self-alignment-opposite-of-anchor)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-alignable-interface
                                coda-mark-interface
                                font-interface
                                mark-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A coda mark.")))))

    (ControlPoint
     . (
        (color . "IndianRed")
        (cross-staff . ,(sticky-grob-interface::inherit-property
                         'cross-staff))
        (horizontal-skylines . #f)
        (layer . 3)
        (text . ,(make-draw-circle-markup 0.3 0.01 #t))
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . #f)
        (X-extent . #f)
        (X-offset . ,(control-point::calc-offset X))
        (Y-extent . #f)
        (Y-offset . ,(control-point::calc-offset Y))
        (meta . ((classes . (Item Spanner))
                 (interfaces . (control-point-interface
                                sticky-grob-interface
                                text-interface))
                 (description . "A visual representation of a
Bézier control point in ties and slurs.")))))

    (ControlPolygon
     . (
        (color . "BurlyWood")
        (cross-staff . ,(sticky-grob-interface::inherit-property
                         'cross-staff))
        (extroversion . 0.5)
        (filled . #f)
        (horizontal-skylines . #f)
        (layer . 2)
        (text . ,control-polygon::calc-text)
        (thickness . 1.2)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . #f)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((classes . (Item Spanner))
                 (interfaces . (control-polygon-interface
                                sticky-grob-interface
                                text-interface))
                 (description . "A visual representation of a
Bézier control polygon as used in ties and slurs.")))))

    (CombineTextScript
     . (
        (avoid-slur . outside)
        (baseline-skip . 2)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-series . bold)
        (outside-staff-priority . 450)
        (padding . 0.5)
        (parent-alignment-X . #f)
        (script-priority . 200)
        (self-alignment-X . #f)
        (side-axis . ,Y)
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface
                                text-script-interface))
                 (description . "A grob for printing markup given
in the @code{soloText}, @code{soloIIText}, and @code{aDueText}
properties if automatic part combining is active.")))))

    (CueClef
     . (
        (avoid-slur . inside)
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-symbol . cue-clef)
        (break-visibility . ,begin-of-line-visible)
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-at-beginning-of-line)
        (font-size . -4)
        (glyph-name . ,ly:clef::calc-glyph-name)
        (non-musical . #t)
        (full-size-change . #t)
        (space-alist . ((signum-repetitionis . (minimum-space . 2.7))
                        (staff-bar . (minimum-space . 2.7))
                        (key-cancellation . (minimum-space . 3.5))
                        (key-signature . (minimum-space . 3.5))
                        (time-signature . (minimum-space . 4.2))
                        (custos . (minimum-space . 0.0))
                        (first-note . (minimum-fixed-space . 3.0))
                        (next-note . (extra-space . 1.0))
                        (right-edge . (extra-space . 0.5))))
        (stencil . ,ly:clef::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (break-aligned-interface
                                clef-interface
                                font-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))
                 (description . "A clef starting a cue.  See also
@iref{Clef}, @iref{ClefModifier}, and @iref{CueEndClef}.")))))

    (CueEndClef
     . (
        (avoid-slur . inside)
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-symbol . cue-end-clef)
        (break-visibility . ,begin-of-line-invisible)
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-at-beginning-of-line)
        (font-size . -4)
        (glyph-name . ,ly:clef::calc-glyph-name)
        (non-musical . #t)
        (full-size-change . #t)
        (space-alist . ((clef . (extra-space . 0.7))
                        (cue-clef . (extra-space . 0.7))
                        (signum-repetitionis . (extra-space . 0.7))
                        (staff-bar . (extra-space . 0.7))
                        (key-cancellation . (minimum-space . 3.5))
                        (key-signature . (minimum-space . 3.5))
                        (time-signature . (minimum-space . 4.2))
                        (first-note . (minimum-fixed-space . 5.0))
                        (next-note . (extra-space . 1.0))
                        (right-edge . (extra-space . 0.5))))
        (stencil . ,ly:clef::print)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (break-aligned-interface
                                clef-interface
                                font-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))
                 (description . "A clef ending a cue.  See also
@iref{Clef}, @iref{ClefModifier}, and @iref{CueClef}.")))))

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
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Item)
                 (interfaces  . (break-aligned-interface
                                 custos-interface
                                 font-interface
                                 staff-symbol-referencer-interface))
                 (description . "A custos, mainly used in older
notation like Gregorian chant.")))))


    ;; Divisio grew out of BreathingSign but is break-aligned like
    ;; BarLine, so it is similar to and different from both of them.
    (Divisio
     . (
        (break-align-anchor
         . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-anchor-alignment . ,CENTER)
        (break-align-symbol . staff-bar)
        (break-visibility . ,begin-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-height . ,item::extra-spacing-height-including-staff)
        (extra-spacing-width . (-1.0 . 0.0))
        (non-musical . #t)
        (space-alist . (
                        (ambitus . (extra-space . 1.0))
                        (time-signature . (extra-space . 0.75))
                        (custos . (minimum-space . 2.0))
                        (clef . (extra-space . 1.0))
                        (key-signature . (extra-space . 1.0))
                        (key-cancellation . (extra-space . 1.0))
                        (first-note . (fixed-space . 1.3))
                        (next-note . (semi-fixed-space . 0.9))
                        (right-edge . (extra-space . 0.0))))
        (stencil . ,ly:text-interface::print)
        (thickness . 1.9)
        (Y-offset . ,(ly:make-unpure-pure-container ly:breathing-sign::offset-callback))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                breathing-sign-interface
                                font-interface
                                outside-staff-interface
                                text-interface))
                 (description . "A structural divider in a chant,
often calling for a breath or caesura.")))))

    (DotColumn
     . (
        (axes . (,X))
        (chord-dots-limit . 3)
        (direction . ,RIGHT)
        (padding . ,dot-column-interface::pad-by-one-dot-width)
        (positioning-done . ,ly:dot-column::calc-positioning-done)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Item)
                 (interfaces . (axis-group-interface
                                dot-column-interface))
                 (description . "An auxiliary grob to align
stacked @iref{Dots} grobs of dotted notes and chords.")))))

    (Dots
     . (
        (avoid-slur . inside)
        (dot-count . ,dots::calc-dot-count)
        (dot-stencil . ,dots::calc-dot-stencil)
        (glyph-name . ,dots::calc-glyph-name)
        (staff-position . ,dots::calc-staff-position)
        (stencil . ,ly:dots::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (extra-spacing-height . (-0.5 . 0.5))
        (extra-spacing-width . (0.0 . 0.2))
        (meta . ((class . Item)
                 (interfaces . (dots-interface
                                font-interface
                                staff-symbol-referencer-interface))
                 (description . "The dot(s) of a dotted note.
See also @iref{DotColumn}.")))))

    (DoublePercentRepeat
     . (
        (break-align-symbol . staff-bar)
        (break-visibility . ,begin-of-line-invisible)
        (dot-negative-kern . 0.75)
        (font-encoding . fetaMusic)
        (non-musical . #t)
        (slash-negative-kern . 1.6)
        (slope . 1.0)
        (stencil . ,ly:percent-repeat-interface::double-percent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                font-interface
                                percent-repeat-interface))
                 (description . "A double-percent symbol for
repeating two bars.  See also @iref{DoublePercentRepeatCounter},
@iref{PercentRepeat}, @iref{DoubleRepeatSlash}, and
@iref{RepeatSlash}.")))))

    (DoublePercentRepeatCounter
     . (
        (direction . ,UP)
        (font-encoding . fetaText)
        (font-features . ("cv47"))
        (font-size . -2)
        (padding . 0.2)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (staff-padding . 0.25)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob to print a counter for
@iref{DoublePercentRepeat} grobs.")))))

    (DoubleRepeatSlash
     . (
        (dot-negative-kern . 0.75)
        (font-encoding . fetaMusic)
        (slash-negative-kern . 1.6)
        (slope . 1.0)
        (stencil . ,ly:percent-repeat-interface::beat-slash)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                outside-staff-interface
                                percent-repeat-interface
                                rhythmic-grob-interface))
                 (description . "A double-percent symbol for
repeating patterns shorter than a single measure, and which
contain mixed durations.  See also @iref{PercentRepeat},
@iref{DoublePercentRepeat}, and @iref{RepeatSlash}.")))))

    (DurationLine
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (arrow-width . 1.5)
        (arrow-length . 2)
        (bound-details
         .
         ((right . ((attach-dir . ,LEFT)
                    (end-on-accidental . #t)
                    (end-on-arpeggio . #t)
                    (padding . 0.4)
                    ;; possible values for endstyle: arrow, hook
                    (end-style . #f)))
          (right-broken . ((padding . 0.4)
                           (end-style . #f)))
          (left-broken . ((padding . 0.5)))
          (left . ((attach-dir . ,RIGHT)
                   (padding . -0.3)
                   (start-at-dot . #f)))))
        (breakable . #t)
        (details
         .
         ((hook-height . 0.34)
          ;; Unless set by the user, grob's thickness is taken as default
          (hook-thickness . #f)
          (hook-direction . ,UP)
          (extra-dot-padding . 0.5)))
        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
        (minimum-length . 2)
        (minimum-length-after-break . 6)
        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,duration-line::print)
        (style . beam)
        (to-barline . #f)
        (thickness . 4)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-offset . 0)
        (zigzag-length . 1)
        (zigzag-width . 1)
        (meta . ((class . Spanner)
                 (interfaces . (horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                duration-line-interface
                                font-interface
                                unbreakable-spanner-interface))
                 (description . "A horizontal duration line,
continuing rhythmic items (usually note heads).")))))

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
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                dynamic-interface
                                dynamic-line-spanner-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "An auxiliary grob providing a
vertical baseline to align successive dynamic grobs
(@iref{DynamicText}, @iref{DynamicTextSpanner},
and @iref{Hairpin}) within a staff.")))))

    (DynamicText
     . (

        ;; todo.

        (direction . ,ly:script-interface::calc-direction)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-encoding . fetaText)
        (font-series . bold)
        (font-shape . italic)
        (parent-alignment-X . ,CENTER)
        (positioning-done . ,ly:script-interface::calc-positioning-done)
        (right-padding . 0.5)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-align-on-main-noteheads . #t)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-offset . ,(scale-by-font-size -0.6)) ; center on an 'm'
        (meta . ((class . Item)
                 (interfaces . (dynamic-interface
                                dynamic-text-interface
                                font-interface
                                outside-staff-interface
                                script-interface
                                self-alignment-interface
                                text-interface))
                 (description . "A dynamic text item like @q{ff}
or @q{mp}.  See also @iref{DynamicLineSpanner}.")))))

    (DynamicTextSpanner
     . (
        (before-line-breaking . ,dynamic-text-spanner::before-line-breaking)
        (bound-details . ((right . ((attach-dir .  ,LEFT)
                                    (padding . 0.75)
                                    ))
                          (right-broken . ((attach-dir .  ,RIGHT)
                                           (padding . 0.0)
                                           ))

                          (left . ((attach-dir .  ,LEFT)
                                   (stencil-offset . (-0.75 . -0.5))
                                   (padding . 0.75)
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

        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info-and-text)

        (minimum-length . 2.0)
        ;; make sure the spanner doesn't get too close to notes
        (minimum-Y-extent . (-1 . 1))

        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (skyline-horizontal-padding . 0.2)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:line-spanner::print)
        (style . dashed-line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (meta . ((class . Spanner)
                 (interfaces . (dynamic-interface
                                dynamic-text-spanner-interface
                                font-interface
                                horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                ;; for now, LilyPond never will typeset
                                ;; these without a DynamicLineSpanner
                                ;; as their controlling element
                                ;; so, they do not need the
                                ;; outside-staff-interface
                                text-interface))
                 (description . "Dynamic text like @q{cresc},
usually followed by a (dashed) line.  See also
@iref{DynamicLineSpanner} and @iref{TextSpanner}.")))))


    (Episema
     . (
        (bound-details . ((left . ((padding . 0)
                                   (attach-dir . ,LEFT)
                                   ))
                          (right . ((padding . 0)
                                    (attach-dir . ,RIGHT)
                                    ))
                          ))
        (direction . ,UP)
        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
        (quantize-position . #t)
        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (side-axis . ,Y)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (episema-interface
                                font-interface
                                horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                side-position-interface))
                 (description . "An @dfn{episema} line (over a
group of notes).  Used in Gregorian chant.")))))


    (Fingering
     . (

        ;; sync with TextScript (?)
        (add-stem-support . ,only-if-beamed)
        (avoid-slur . around)
        (cross-staff . ,script-or-side-position-cross-staff)
        (direction . ,ly:script-interface::calc-direction)
        (font-encoding . fetaText)
        (font-features . ("cv47" "ss01"))
        (font-size . -5)                ; don't overlap when next to heads.
        (padding . 0.5)
        (parent-alignment-X . ,CENTER)
        (parent-alignment-Y . ,CENTER)
        (positioning-done . ,ly:script-interface::calc-positioning-done)
        (script-priority . 100)
        (self-alignment-X . ,CENTER)
        (self-alignment-Y . ,CENTER)
        (slur-padding . 0.2)
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (text . ,fingering::calc-text)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (finger-interface
                                font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface
                                text-script-interface))
                 (description . "A fingering symbol (usually a
digit).  See also @iref{FingeringColumn} and
@iref{StrokeFinger}.")))))

    (FingeringColumn
     . (
        (padding . 0.2)
        (positioning-done . ,ly:fingering-column::calc-positioning-done)
        (snap-radius . 0.3)
        (meta . ((class . Item)
                 (interfaces . (fingering-column-interface))
                 (description . "An auxiliary grob to align
stacked @iref{Fingering} grobs.")))))

    (FingerGlideSpanner
     . ((bound-details . ((right . ((attach-dir .  ,LEFT)
                                    (right-stub-length . 1)
                                    (padding . 0.2)
                                    ))
                          (left . ((attach-dir .  ,RIGHT)
                                   (left-stub-length . 1)
                                   (padding . 0.2)
                                   ))
                          ))
        (dash-fraction . 0.4)
        (dash-period . 1)
        (details . ((bow-direction . #f)))
        (left-bound-info . ,ly:line-spanner::calc-left-bound-info)
        (normalized-endpoints . ,ly:spanner::calc-normalized-endpoints)
        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (minimum-length . 2.5)
        (minimum-length-after-break . 2.5)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,finger-glide::print)
        (style . line)
        (thickness . 1.4)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (zigzag-length . 1)
        (zigzag-width . 1)
        (meta . ((class . Spanner)
                 (interfaces . (finger-glide-interface
                                line-spanner-interface))
                 (description . "A line connecting two
@iref{Fingering} grobs, usually indicating a gliding finger
for stringed instruments.")))))

    (Flag
     . (
        (glyph-name . ,ly:flag::glyph-name)
        (stencil . ,ly:flag::print)
        (transparent . ,(grob::inherit-parent-property
                         X 'transparent))
        (color . ,(grob::inherit-parent-property
                   X 'color))
        (X-extent . ,ly:flag::width)
        (X-offset . ,ly:flag::calc-x-offset)
        (Y-offset . ,(ly:make-unpure-pure-container ly:flag::calc-y-offset ly:flag::pure-calc-y-offset))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (flag-interface
                                font-interface))
                 (description . "A flag (in the musical
sense).")))))

    (Footnote
     . (
        (after-line-breaking . ,ly:balloon-interface::remove-irrelevant-spanner)
        (annotation-balloon . #f)
        (annotation-line . #t)
        (automatically-numbered . ,(grob::calc-property-by-copy 'automatically-numbered))
        (break-visibility . ,(sticky-grob-interface::inherit-property
                              'break-visibility))
        (cross-staff . ,(sticky-grob-interface::inherit-property
                         'cross-staff))
        (footnote . #t)
        (footnote-text . ,(grob::calc-property-by-copy 'footnote-text))
        (spanner-placement . ,LEFT)
        (stencil . ,ly:balloon-interface::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (X-extent . #f)
        (Y-extent . #f)
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (meta . ((classes . (Item Spanner))
                 (interfaces . (balloon-interface
                                footnote-interface
                                font-interface
                                sticky-grob-interface
                                text-interface))
                 (description . "A footnote mark (usually a
number) with a pointing line attached to another grob.")))))

    (FretBoard
     . (
        (after-line-breaking . ,ly:chord-name::after-line-breaking)
        (fret-diagram-details . ((finger-code . below-string)))
        (stencil . ,fret-board::calc-stencil)
        (extra-spacing-height . (0.2 . -0.2))
        (extra-spacing-width . (-0.5 . 0.5))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (chord-name-interface
                                font-interface
                                fret-diagram-interface
                                outside-staff-interface
                                rhythmic-grob-interface))
                 (description . "A fretboard diagram.")))))


    (Glissando
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (bound-details . ((right . ((attach-dir .  ,LEFT)
                                    (end-on-accidental . #t)
                                    (padding . 0.5)
                                    ))
                          (left . ((attach-dir .  ,RIGHT)
                                   (padding . 0.5)
                                   (start-at-dot . #t)
                                   ))
                          ))
        (cross-staff . ,ly:line-spanner::calc-cross-staff)
        (gap . 0.5)
        (left-bound-info . ,ly:line-spanner::calc-left-bound-info)
        (normalized-endpoints . ,ly:spanner::calc-normalized-endpoints)
        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (zigzag-width . 0.75)
        (meta . ((class . Spanner)
                 (interfaces . (glissando-interface
                                line-interface
                                line-spanner-interface
                                unbreakable-spanner-interface))
                 (description . "A glissando line.")))))

    (GraceSpacing
     . (
        (common-shortest-duration . ,grace-spacing::calc-shortest-duration)
        (shortest-duration-space . 1.6)
        (spacing-increment . 0.8)
        (meta . ((class . Spanner)
                 (interfaces . (grace-spacing-interface
                                spacing-options-interface))
                 (description . "An auxiliary grob to handle
(horizontal) spacing of grace notes.  See also
@iref{NoteSpacing}, @iref{StaffSpacing},
and @iref{SpacingSpanner}.")))))

    ;; We could reuse ChordName and make it spanner in ChordGrid and item
    ;; elsewhere, but the defaults seem sufficiently different to warrant
    ;; a separate grob.
    (GridChordName
     . (
        (font-family . sans)
        (font-size . 1.5)
        (stencil . ,ly:text-interface::print)
        (word-space . 0.0)
        (X-offset . ,grid-chord-name::calc-X-offset)
        (Y-offset . ,grid-chord-name::calc-Y-offset)
        (meta . ((class . Spanner)
                 (interfaces . (accidental-switch-interface
                                grid-chord-name-interface
                                font-interface
                                text-interface))
                 (description . "A chord name in a chord grid.")))))

    (GridLine
     . (
        (layer . 0)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:grid-line-interface::print)
        (X-extent . ,ly:grid-line-interface::width)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (meta . ((class . Item)
                 (interfaces . (grid-line-interface
                                self-alignment-interface))
                 (description . "A vertical line between staves,
indicating rhythmic synchronization.  See also
@iref{GridPoint}.")))))

    (GridPoint
     . (
        (X-extent . (0 . 0))
        (Y-extent . (0 . 0))
        (meta . ((class . Item)
                 (interfaces . (grid-point-interface))
                 (description . "An auxiliary grob marking a
start or end point for a @iref{GridLine} grob.")))))


    (Hairpin
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (bound-padding . 1.0)
        (broken-bound-padding . ,ly:hairpin::broken-bound-padding)
        (circled-tip . #f)
        (endpoint-alignments . (,LEFT . ,RIGHT))
        (grow-direction . ,hairpin::calc-grow-direction)
        (height . 0.6666)
        (minimum-length . 2.0)
        (self-alignment-Y . ,CENTER)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:hairpin::print)
        (thickness . 1.0)
        (to-barline . #t)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-extent . ,(grob::unpure-Y-extent-from-stencil ly:hairpin::pure-height))
        (Y-offset . ,self-alignment-interface::y-aligned-on-self)
        (meta . ((class . Spanner)
                 (interfaces . (dynamic-interface
                                hairpin-interface
                                line-interface
                                outside-staff-interface
                                self-alignment-interface))
                 (description . "A hairpin.  See also
@iref{DynamicLineSpanner}.")))))

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
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (horizontal-bracket-interface
                                line-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "A horizontal bracket between
notes.  See also @iref{HorizontalBracketText} and
@iref{MeasureSpanner}.")))))

    (HorizontalBracketText
     . (
        (direction . ,ly:horizontal-bracket-text::calc-direction)
        (font-size . -1)
        (padding . 0.5)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (stencil . ,ly:horizontal-bracket-text::print)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                horizontal-bracket-text-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "Text (markup) for a
@iref{HorizontalBracket} grob.")))))


    (InstrumentName
     . (
        (direction . ,LEFT)
        (padding . 0.3)
        (self-alignment-X . ,CENTER)
        (self-alignment-Y . ,CENTER)
        (stencil . ,system-start-text::print)
        (X-offset . ,system-start-text::calc-x-offset)
        (Y-offset . ,system-start-text::calc-y-offset)
        (meta . ((class . Spanner)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                self-alignment-interface
                                side-position-interface
                                system-start-text-interface
                                text-interface))
                 (description . "An instrument name, usually
displayed to the left of a staff.")))))

    (InstrumentSwitch
     . (
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (outside-staff-priority . 500)
        (padding . 0.5)
        (parent-alignment-X . #f)
        (self-alignment-X . ,LEFT)
        (side-axis . ,Y)
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "This grob is deprecated.  Do
not use it.")))))


    ;; This is similar to a rehearsal mark, but is intended for a
    ;; "point of departure" such as D.C.  See Behind Bars, pp. 238-240
    ;; for style advice.
    (JumpScript
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (staff-bar key-signature clef))
        (break-visibility . ,begin-of-line-invisible)
        (direction . ,DOWN) ; expect users to override to UP in vocal scores
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-shape . italic)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 1350) ; slightly lower than SegnoMark,CodaMark
        (padding . 0.8)
        (self-alignment-X . ,RIGHT)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-alignable-interface
                                font-interface
                                jump-script-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob to display a @q{point of
departure} like @emph{D.C. al fine}.")))))


    (KeyCancellation
     . (
        (break-align-symbol . key-cancellation)
        (break-visibility . ,begin-of-line-invisible)
        (non-musical . #t)
        (flat-positions . (2 3 4 2 1 2 1))
        (sharp-positions . (4 5 4 2 3 2 3))
        (space-alist . (
                        (time-signature . (extra-space . 1.25))
                        (signum-repetitionis . (extra-space . 0.6))
                        (staff-bar . (extra-space . 0.6))
                        (key-signature . (extra-space . 0.5))
                        (cue-clef . (extra-space . 0.5))
                        (right-edge . (extra-space . 0.5))
                        (first-note . (fixed-space . 2.5))
                        (custos . (extra-space . 1.0))))
        (stencil . ,ly:key-signature-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (extra-spacing-width . (0.0 . 1.0))
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-including-staff)
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (accidental-switch-interface
                                break-aligned-interface
                                font-interface
                                key-cancellation-interface
                                key-signature-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))
                 (description . "A key cancellation, normally
consisting of naturals, to be displayed (if necessary) immediately
before a @iref{KeySignature} grob if the key changes.")))))

    (KeySignature
     . (
        (avoid-slur . inside)
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-anchor-alignment . ,RIGHT)
        (break-align-symbol . key-signature)
        (break-visibility . ,begin-of-line-visible)
        (non-musical . #t)
        (flat-positions . (2 3 4 2 1 2 1))
        (sharp-positions . (4 5 4 2 3 2 3))
        (space-alist . (
                        (ambitus . (extra-space . 1.15))
                        (time-signature . (extra-space . 1.15))
                        (signum-repetitionis . (extra-space . 1.1))
                        (staff-bar . (extra-space . 1.1))
                        (cue-clef . (extra-space . 0.5))
                        (right-edge . (extra-space . 0.5))
                        (first-note . (fixed-space . 2.5))))
        (stencil . ,ly:key-signature-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (extra-spacing-width . (0.0 . 1.0))
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-including-staff)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (accidental-switch-interface
                                break-aligned-interface
                                font-interface
                                key-signature-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))
                 (description . "A key signature.  See also
@iref{KeyCancellation}.")))))

    (KievanLigature
     . (
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:kievan-ligature::print)
        (padding . 0.5)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                kievan-ligature-interface))
                 (description . "An auxiliary grob to handle a
melisma (ligature) as used in Kievan square notation.  See also
@iref{MensuralLigature}, @iref{VaticanaLigature}, and
@iref{LigatureBracket}.")))))


    (LaissezVibrerTie
     . (
        (control-points . ,ly:semi-tie::calc-control-points)
        (cross-staff . ,semi-tie::calc-cross-staff)
        (details . ((ratio . 0.333)
                    (height-limit . 1.0)))
        (direction . ,ly:tie::calc-direction)
        (head-direction . ,LEFT)
        (stencil  . ,ly:tie::print)
        (thickness . 1.0)
        (extra-spacing-height . (-0.5 . 0.5))
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (bezier-curve-interface
                                semi-tie-interface
                                tie-interface))
                 (description . "A laissez-vibrer tie (i.e., a
tie from a note into nothing).  See also
@iref{LaissezVibrerTieColumn}, @iref{RepeatTie}, and
@iref{Tie}.")))))

    (LaissezVibrerTieColumn
     . (
        (head-direction . ,ly:semi-tie-column::calc-head-direction)
        (positioning-done . ,ly:semi-tie-column::calc-positioning-done)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Item)
                 (interfaces . (semi-tie-column-interface))
                 (description . "An auxiliary grob to determine
direction and shape of stacked @iref{LaissezVibrerTie}
grobs.")))))

    (LedgerLineSpanner
     . (
        (layer . 0)
        (length-fraction . 0.25)
        (minimum-length-fraction . 0.25)
        (springs-and-rods . ,ly:ledger-line-spanner::set-spacing-rods)
        (stencil . ,ly:ledger-line-spanner::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Spanner)
                 (interfaces . (ledger-line-spanner-interface))
                 (description . "An auxiliary grob to manage
ledger lines of a whole staff.")))))

    (LeftEdge
     . (
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-symbol . left-edge)
        (break-visibility . ,begin-of-line-visible)
        (non-musical . #t)
        (space-alist . (
                        (ambitus . (extra-space . 1.15))
                        (breathing-sign . (minimum-space . 0.0))
                        (cue-end-clef . (extra-space . 0.8))
                        (clef . (extra-space . 0.8))
                        (cue-clef . (extra-space . 0.8))
                        (signum-repetitionis . (extra-space . 0.0))
                        (staff-bar . (extra-space . 0.0))
                        (staff-ellipsis . (extra-space . 0.0))
                        (key-cancellation . (extra-space . 0.0))
                        (key-signature . (extra-space . 0.8))
                        (time-signature . (extra-space . 1.0))
                        (custos . (extra-space . 0.0))
                        (first-note . (fixed-space . 2.0))
                        (right-edge . (extra-space . 0.0))
                        ))
        (X-extent . (0 . 0))
        (Y-extent . (0 . 0))
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface))
                 (description . "The left edge of a staff.  Useful
as an anchor point for other grobs.")))))

    (LigatureBracket
     . (
        ;; ugh.  A ligature bracket is totally different from
        ;; a tuplet bracket.

        (bracket-visibility . #t)
        (connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
        (direction . ,UP)
        (edge-height . (0.7 . 0.7))
        (padding . 2.0)
        (positions . ,ly:tuplet-bracket::calc-positions)
        (shorten-pair . (-0.2 . -0.2))
        (staff-padding . 0.25)
        (stencil . ,ly:tuplet-bracket::print)
        (thickness . 1.6)
        (tuplet-slur . #f)
        (X-positions . ,ly:tuplet-bracket::calc-x-positions)
        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                tuplet-bracket-interface))
                 (description . "A horizontal bracket over a group
of notes, usually indicating an ancient ligature if transcribed
into modern notation.  See also @iref{KievanLigature},
@iref{MensuralLigature}, and @iref{VaticanaLigature}.")))))

    (LyricExtender
     . (
        (minimum-length . 1.5)
        (stencil . ,ly:lyric-extender::print)
        (thickness . 0.8) ; line-thickness
        (Y-extent . (0 . 0))
        (meta . ((class . Spanner)
                 (interfaces . (lyric-extender-interface
                                lyric-interface))
                 (description . "An extender line in lyrics.")))))

    (LyricHyphen
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (dash-period . 10.0)
        (height . 0.42)
        (length . 0.66)
        (minimum-distance . 0.1)
        (minimum-length . 0.3)
        (padding . 0.07)
        (springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
        (stencil . ,ly:lyric-hyphen::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (thickness . 1.3)
        (Y-extent . (0 . 0))
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                lyric-hyphen-interface
                                lyric-interface))
                 (description . "A hyphen in lyrics.  See also
@iref{VowelTransition}.")))))

    (LyricRepeatCount
     . (
        (break-align-symbols . (staff-bar breathing-sign))
        (break-visibility . ,begin-of-line-invisible)
        (extra-spacing-width . (-1.0 . 1.0))
        ;; Recede in height for purposes of note spacing,
        ;; so notes in melismata can be freely spaced above lyrics
        (extra-spacing-height . (0.2 . -0.2))
        (font-series . medium)
        (font-shape . italic)
        (font-size . 1.0)
        (non-musical . #t)
        ;; To help communicate that the count describes the section
        ;; that is ending, keep it left of center at a double repeat.
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,RIGHT)
        (stencil . ,lyric-text::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (word-space . 0.6)
        (skyline-horizontal-padding . 0.1)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (description . "A repeat count in lyrics.")
                 (interfaces . (break-alignable-interface
                                font-interface
                                lyric-interface
                                lyric-repeat-count-interface
                                self-alignment-interface
                                text-interface))))))

    (LyricSpace
     . (
        (minimum-distance . 0.45)
        (padding . 0.0)
        (springs-and-rods . ,ly:lyric-hyphen::set-spacing-rods)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Spanner)
                 (interfaces . (lyric-hyphen-interface
                                lyric-space-interface))
                 (description . "A space in lyrics.")))))

    (LyricText
     . (
        (extra-spacing-width . (0.0 . 0.0))
        ;; Recede in height for purposes of note spacing,
        ;; so notes in melismata can be freely spaced above lyrics
        (extra-spacing-height . (0.2 . -0.2))
        (font-series . medium)
        (font-size . 1.0)
        (parent-alignment-X . ())
        (self-alignment-X . ,CENTER)
        (stencil . ,lyric-text::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (word-space . 0.6)
        (skyline-horizontal-padding . 0.1)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-align-on-main-noteheads . #t)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                lyric-syllable-interface
                                rhythmic-grob-interface
                                self-alignment-interface
                                text-interface))
                 (description . "A chunk of text in lyrics.  See
also @iref{LyricExtender}, @iref{LyricHyphen}, @iref{LyricSpace},
and @iref{VowelTransition}.")))))


    (MeasureCounter
     . (
        (count-from . 1)
        (direction . ,UP)
        (font-encoding . fetaText)
        (font-features . ("cv47"))
        (font-size . -2)
        ;; This ought to be an en dash rather than a mere dash, but it
        ;; looks way too wide with fetaText.
        (number-range-separator . "-")
        (outside-staff-horizontal-padding . 0.5)
        (outside-staff-priority . 750)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (spacing-pair . (break-alignment . break-alignment))
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (text . ,measure-counter::text)
        ;; For the space around the number-range-separator.
        (word-space . 0.2)
        (X-offset . ,centered-spanner-interface::calc-x-offset)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (centered-spanner-interface
                                font-interface
                                measure-counter-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob to print a counter for
measures.")))))

    (MeasureSpanner
     . (
        (connect-to-neighbor . ,ly:measure-spanner::calc-connect-to-neighbors)
        (direction . ,UP)
        (edge-height . (0.7 . 0.7))
        (outside-staff-priority . 750)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (spacing-pair . (staff-bar . staff-bar))
        (staff-padding . 0.5)
        (stencil . ,ly:measure-spanner::print)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                measure-spanner-interface
                                line-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A horizontal bracket between
bar lines.  See also @iref{HorizontalBracket}.")))))

    (MeasureGrouping
     . (
        (direction . ,UP)
        (height . 2.0)
        (padding . 2)
        (side-axis . ,Y)
        (staff-padding . 3)
        (stencil . ,ly:measure-grouping::print)
        (thickness . 1)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (measure-grouping-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "A measure grouping or conducting
sign.")))))

    (MelodyItem
     . (
        (neutral-direction . ,DOWN)
        (meta . ((class . Item)
                 (interfaces . (melody-spanner-interface))
                 (description . "An auxiliary grob to help alter
the stem directions of middle notes on a staff so that they
follow the melody.")))))

    (MensuralLigature
     . (
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:mensural-ligature::print)
        (thickness . 1.3)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                mensural-ligature-interface))
                 (description . "A grob to display a ligature as
used in mensural notation.  See also @iref{KievanLigature},
@iref{VaticanaLigature}, and @iref{LigatureBracket}.")))))

    (MetronomeMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (break-visibility . ,end-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (flag-style . default)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 1300)
        (padding . 0.8)
        (side-axis . ,Y)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (self-alignment-X . ,LEFT)
        (break-align-symbols . (time-signature))
        (non-break-align-symbols . (paper-column-interface))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-alignable-interface
                                font-interface
                                metronome-mark-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A metronome mark.  This is
either a precise tempo indication like @q{quarter note = 80}, or
an arbitrary piece of text (like @q{Allegro}), possibly followed
by a precise indication in parentheses.")))))

    (MultiMeasureRest
     . (
        (expand-limit . 10)
        (hair-thickness . 2.0)
        (round-up-exceptions . ())
        (bound-padding . 0.5)
        (max-symbol-separation . 8.0)
        (space-increment . 2.0)
        (spacing-pair . (break-alignment . break-alignment))
        (springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
        (stencil . ,ly:multi-measure-rest::print)
        (thick-thickness . 6.6)
        ;; See Wanske pp. 125
        (usable-duration-logs . ,(iota 4 -3))
        (voiced-position . 4)
        (Y-extent . ,(ly:make-unpure-pure-container ly:multi-measure-rest::height))
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                multi-measure-interface
                                outside-staff-interface
                                multi-measure-rest-interface
                                rest-interface
                                staff-symbol-referencer-interface))
                 (description . "A multi-measure rest.  See also
@iref{MultiMeasureRestNumber}, @iref{MultiMeasureRestText},
@iref{MultiMeasureRestScript}, and @iref{Rest}.")))))

    (MultiMeasureRestNumber
     . (
        (bound-padding  . 1.0)
        (direction . ,UP)
        (font-encoding . fetaText)
        (font-features . ("cv47"))
        (padding . 0.4)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (springs-and-rods . ,ly:multi-measure-rest::set-text-rods)
        (staff-padding . 0.4)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                multi-measure-interface
                                multi-measure-rest-number-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob to print the length of a
@iref{MultiMeasureRest} grob.")))))

    (MultiMeasureRestText
     . (
        (direction . ,UP)
        (outside-staff-priority . 450)
        (padding . 0.2)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (skyline-horizontal-padding . 0.2)
        (staff-padding . 0.25)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                multi-measure-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A text markup for a
@iref{MultiMeasureRest} grob.  See also @iref{TextScript}.")))))

    (MultiMeasureRestScript
     . (
        (direction . ,UP)
        (outside-staff-padding . 0)
        (outside-staff-priority . 40)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (staff-padding . 0.25)
        (stencil . ,ly:script-interface::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                multi-measure-interface
                                outside-staff-interface
                                script-interface
                                self-alignment-interface
                                side-position-interface))
                 (description . "An articulation (like a fermata)
attached to a @iref{MultiMeasureRest} grob.  See also
@iref{Script}.")))))


    (NonMusicalPaperColumn
     . (
        (allow-loose-spacing . #t)
        (axes . (,X))
        (bound-alignment-interfaces . (break-alignment-interface))
        ;; used by Paper_column::print when debugging columns:
        (font-size . -7.5)
        (full-measure-extra-space . 1.0)
        (horizontal-skylines . ,ly:separation-item::calc-skylines)
        ;;                    (stencil . ,ly:paper-column::print)

        (keep-inside-line . #t)
        ;; used by Paper_column::print when debugging columns:
        (layer . 1000)
        (line-break-permission . allow)
        (non-musical . #t)
        (page-break-permission . allow)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Paper_column)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                font-interface
                                non-musical-paper-column-interface
                                separation-item-interface
                                spaceable-grob-interface))
                 (description . "An auxiliary grob grouping
non-musical items to handle the flexible horizontal space between
non-musical and musical columns.  Grobs that have the property
@code{non-musical} set to @code{#t} belong to this column.")))))

    (NoteCollision
     . (
        (axes . (,X ,Y))
        (fa-styles . (fa faFunk faThin faWalker))
        (note-collision-threshold . 1)
        (positioning-done . ,ly:note-collision-interface::calc-positioning-done)
        (prefer-dotted-right . #t)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                note-collision-interface))
                 (description . "An auxiliary grob to group
@iref{NoteColumn} grobs from several voices, mainly to handle
note collisions.  See also @iref{RestCollision}.")))))

    (NoteColumn
     . (
        (bend-me . ())
        (axes . (,X ,Y))
        (bound-alignment-interfaces . (rhythmic-head-interface stem-interface))
        (horizontal-skylines . ,ly:separation-item::calc-skylines)
        (main-extent . ,ly:note-column::calc-main-extent)
        (skyline-vertical-padding . 0.15)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                bend-interface
                                note-column-interface
                                separation-item-interface))
                 (description . "An auxiliary grob to align
stacked notes, stems, flags, accidentals, and other items from
the same voice.  See also @iref{NoteCollision}.")))))

    (NoteHead
     . (
        (bend-me . ())
        (flexa-width . 2.0)
        (duration-log . ,note-head::calc-duration-log)
        (extra-spacing-height . ,ly:note-head::include-ledger-line-height)
        (glyph-name . ,note-head::calc-glyph-name)
        (ligature-flexa . #f)
        (parenthesis-friends . (accidental-grob dot))
        (stem-attachment . ,ly:note-head::calc-stem-attachment)
        (stencil . ,ly:note-head::print)
        (X-offset . ,ly:note-head::stem-x-shift)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (bend-interface
                                font-interface
                                gregorian-ligature-interface
                                ledgered-interface
                                ligature-head-interface
                                mensural-ligature-interface
                                note-head-interface
                                rhythmic-grob-interface
                                rhythmic-head-interface
                                staff-symbol-referencer-interface
                                vaticana-ligature-interface))
                 (description . "A note head.  See also
@iref{TabNoteHead}.")))))

    (NoteName
     . (
        (stencil . ,ly:text-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                note-name-interface
                                text-interface))
                 (description . "A textual representation of a
note name.")))))

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
                                spacing-interface))
                 (description . "An auxiliary grob to handle
(horizontal) spacing of notes.  See also
@iref{GraceSpacing}, @iref{StaffSpacing}, and
@iref{SpacingSpanner}.")))))


    (OttavaBracket
     . (
        (dash-fraction . 0.3)
        (edge-height . (0 . 0.8))
        (font-series . bold)
        (font-shape . italic)
        (minimum-length . 0.3)
        (outside-staff-priority . 400)
        (padding . 0.5)
        (shorten-pair . (-0.8 . -0.6))
        (staff-padding . 2.0)
        (stencil . ,ly:ottava-bracket::print)
        (style . dashed-line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                horizontal-bracket-interface
                                line-interface
                                outside-staff-interface
                                ottava-bracket-interface
                                side-position-interface
                                text-interface))
                 (description . "An ottava bracket.")))))


    (PaperColumn
     . (
        (allow-loose-spacing . #t)
        (axes . (,X))
        (bound-alignment-interfaces . (note-column-interface))
        ;; used by Paper_column::print when debugging columns:
        (font-size . -7.5)
        (horizontal-skylines . ,ly:separation-item::calc-skylines)
        (keep-inside-line . #t)
        ;; used by Paper_column::print when debugging columns:
        (layer . 1000)
        ;; 0.08 comes from spacing-horizontal-skyline.ly
        ;; allows double flat of F to be nestled over dots of C
        (skyline-vertical-padding . 0.08)
        ;; (stencil . ,ly:paper-column::print)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Paper_column)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                font-interface
                                musical-paper-column-interface
                                separation-item-interface
                                spaceable-grob-interface))
                 (description . "An auxiliary grob grouping
musical items to handle the flexible horizontal space between
musical and non-musical columns.  See also
@iref{NonMusicalPaperColumn}.")))))

    (Parentheses
     . (
        (break-visibility . ,(sticky-grob-interface::inherit-property
                              'break-visibility))
        (font-size . -6)
        (padding . 0.2)
        (stencil . ,parentheses-interface::print)
        (stencils . ,parentheses-interface::calc-parenthesis-stencils)
        ;; X-extent needs to be non-empty in order to allow proper
        ;; horizontal attachment.  Parentheses does not reserve
        ;; space of its own, however.
        (X-extent . (0 . 0))
        (Y-extent . ,parentheses-interface::y-extent)
        (meta . ((classes . (Item Spanner))
                 (interfaces . (font-interface
                                parentheses-interface
                                sticky-grob-interface))
                 (description . "A grob to create parentheses
around other grobs.")))))

    (PercentRepeat
     . (
        (dot-negative-kern . 0.75)
        (font-encoding . fetaMusic)
        (self-alignment-X . ,CENTER)
        (slope . 1.0)
        (spacing-pair . (break-alignment . staff-bar))
        (springs-and-rods . ,ly:multi-measure-rest::set-spacing-rods)
        (stencil . ,ly:percent-repeat-interface::percent)
        (thickness . 0.48)
        (X-offset . ,centered-spanner-interface::calc-x-offset)
        (meta . ((class . Spanner)
                 (interfaces . (centered-spanner-interface
                                font-interface
                                multi-measure-rest-interface
                                percent-repeat-interface))
                 (description . "A percent symbol for repeating
a bar.  See also @iref{PercentRepeatCounter},
@iref{DoublePercentRepeat}, @iref{DoubleRepeatSlash}, and
@iref{RepeatSlash}.")))))

    (PercentRepeatCounter
     . (
        (direction . ,UP)
        (font-encoding . fetaText)
        (font-features . ("cv47"))
        (font-size . -2)
        (padding . 0.2)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (staff-padding . 0.25)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A grob to print a counter for
@iref{PercentRepeat} grobs.")))))

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
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-extent . ,slur::height)
        (meta . ((class . Spanner)
                 (interfaces . (bezier-curve-interface
                                outside-staff-interface
                                slur-interface))
                 (description . "A phrasing slur, indicating a
@q{musical sentence}.  See also @iref{Slur}.")))))

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
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                piano-pedal-bracket-interface
                                piano-pedal-interface))
                 (description . "A piano pedal bracket.  It can
also be part of @iref{SostenutoPedal}, @iref{SustainPedal}, or
@iref{UnaCordaPedal} grobs if they are printed in a bracketed
style.")))))


    (RehearsalMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (staff-bar key-signature clef))
        (break-visibility . ,end-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-size . 2)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 1500)
        (padding . 0.8)
        (self-alignment-X . ,break-alignable-interface::self-alignment-opposite-of-anchor)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                break-alignable-interface
                                font-interface
                                mark-interface
                                outside-staff-interface
                                rehearsal-mark-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A rehearsal mark.")))))

    (RepeatSlash
     . (
        (slash-negative-kern . 0.85)
        (slope . 1.7)
        (stencil . ,ly:percent-repeat-interface::beat-slash)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (percent-repeat-interface
                                rhythmic-grob-interface))
                 (description . "A symbol consisting of one or
more slashes for repeating patterns shorter than a single
measure, and which contain identical durations.  See also
@iref{PercentRepeat}, @iref{DoublePercentRepeat}, and
@iref{DoubleRepeatSlash}.")))))

    (RepeatTie
     . (
        (cross-staff . ,semi-tie::calc-cross-staff)
        (control-points . ,ly:semi-tie::calc-control-points)
        (details . ((ratio . 0.333)
                    (height-limit . 1.0)))
        (direction . ,ly:tie::calc-direction)
        (head-direction . ,RIGHT)
        (stencil  . ,ly:tie::print)
        (thickness . 1.0)
        (extra-spacing-height . (-0.5 . 0.5))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (bezier-curve-interface
                                semi-tie-interface
                                tie-interface))
                 (description . "A repeat tie (i.e., a tie from
nothing to a note).  See also @iref{RepeatTieColumn},
@iref{LaissezVibrerTie}, and @iref{Tie}.")))))

    (RepeatTieColumn
     . (
        (head-direction . ,ly:semi-tie-column::calc-head-direction)
        (positioning-done . ,ly:semi-tie-column::calc-positioning-done)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Item)
                 (interfaces . (semi-tie-column-interface))
                 (description . "An auxiliary grob to determine
direction and shape of stacked @iref{RepeatTie} grobs.")))))

    (Rest
     . (
        (cross-staff . ,ly:rest::calc-cross-staff)
        (duration-log . ,stem::calc-duration-log)
        (minimum-distance . 0.25)
        (parenthesis-friends . (dot))
        (stencil . ,ly:rest::print)
        (voiced-position . 4)
        (X-extent . ,ly:rest::width)
        (Y-extent . ,(ly:make-unpure-pure-container ly:rest::height ly:rest::pure-height))
        (Y-offset . ,(ly:make-unpure-pure-container ly:rest::y-offset-callback))
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                rest-interface
                                rhythmic-grob-interface
                                rhythmic-head-interface
                                staff-symbol-referencer-interface))
                 (description . "An ordinary rest.  See also
@iref{MultiMeasureRest}.")))))

    (RestCollision
     . (
        (minimum-distance . 0.75)
        (positioning-done . ,ly:rest-collision::calc-positioning-done)
        (meta . ((class . Item)
                 (interfaces . (rest-collision-interface))
                 (description . "An auxiliary grob to handle rest
collisions of different voices.  See also
@iref{NoteCollision}.")))))


    (Script
     . (
        (add-stem-support . #t)
        (cross-staff . ,ly:script-interface::calc-cross-staff)
        (direction . ,ly:script-interface::calc-direction)
        (font-encoding . fetaMusic)
        (horizon-padding . 0.1) ; to avoid interleaving with accidentals
        (positioning-done . ,ly:script-interface::calc-positioning-done)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)

        ;; padding set in script definitions.
        (slur-padding . 0.2)
        (staff-padding . 0.25)

        (stencil . ,ly:script-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-offset . ,script-interface::calc-x-offset)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                outside-staff-interface
                                script-interface
                                self-alignment-interface
                                side-position-interface))
                 (description . "An articulation (staccato,
accent, etc.).  See also @iref{ScriptColumn},
@iref{ScriptRow}, and @iref{MultiMeasureRestScript}.")))))

    (ScriptColumn
     . (
        (before-line-breaking . ,ly:script-column::before-line-breaking)
        (meta . ((class . Item)
                 (interfaces . (script-column-interface))
                 (description . "An auxiliary grob to
(vertically) align stacked @iref{Script} grobs.")))))

    (ScriptRow
     . (
        (before-line-breaking . ,ly:script-column::row-before-line-breaking)
        (meta . ((class . Item)
                 (interfaces . (script-column-interface))
                 (description . "An auxiliary grob to
horizontally align stacked @iref{Script} grobs.")))))

    (SectionLabel
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (left-edge staff-bar))
        (break-visibility . ,end-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        ;; Larger than MetronomeMark; smaller than RehearsalMark.
        ;; Avoid bold to contrast with descriptive tempo marks.
        (font-size . 1.5)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        ;; SectionLabel was developed as an optional substitute for an
        ;; automatic CodaMark, so they are unlikely to be
        ;; simultaneous, but if a long SectionLabel overlaps a later
        ;; SegnoMark or CodaMark, it looks better to place the
        ;; SectionLabel outside.
        ;;
        ;; We keep RehearsalMarks outside by default because their
        ;; nominal function is outside of the music.
        (outside-staff-priority . 1450)
        (padding . 0.8)
        (self-alignment-X . ,LEFT)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-alignable-interface
                                font-interface
                                outside-staff-interface
                                section-label-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A section label, for example
@q{Coda}.")))))

    (SegnoMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (staff-bar key-signature clef))
        (break-visibility . ,end-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-size . 2)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 1400) ; inside RehearsalMark
        (padding . 0.8)
        (self-alignment-X . ,break-alignable-interface::self-alignment-opposite-of-anchor)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-alignable-interface
                                font-interface
                                mark-interface
                                outside-staff-interface
                                segno-mark-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))
                 (description . "A segno mark (created with
@code{\\repeat segno}, not with @code{\\segno}).")))))

    ;; Ancient end-repeat sign.  This isn't a bar line, but it shares
    ;; much of the BarLine implementation.
    (SignumRepetitionis
     . (
        (bar-extent . ,ly:bar-line::calc-bar-extent)
        (break-align-anchor . ,ly:bar-line::calc-anchor)
        (break-align-symbol . signum-repetitionis)
        (break-visibility . ,begin-of-line-invisible)
        (extra-spacing-height . ,pure-from-neighbor-interface::account-for-span-bar)
        (gap . 0.4)
        ;; The engraver should set a glyph based on the repeat count.
        ;; If it doesn't, we signal it with this modern-looking placeholder.
        (glyph . ":|.")
        ;; Bypass BarLine's start-/middle-/end-of-line distinctions.
        (glyph-name . ,(grob::relay-other-property 'glyph))
        (kern . 3.0)
        (segno-kern . 3.0)
        (hair-thickness . 1.9)
        (thick-thickness . 6.0)

        (layer . 0)
        (non-musical . #t)
        (rounded . #f)
        (space-alist . ((ambitus . (extra-space . 1.0))
                        (time-signature . (extra-space . 0.75))
                        (custos . (minimum-space . 2.0))
                        (clef . (extra-space . 1.0))
                        (key-signature . (extra-space . 1.0))
                        (key-cancellation . (extra-space . 1.0))
                        (first-note . (extra-space . 0.5))
                        (next-note . (semi-fixed-space . 0.9))
                        (signum-repetitionis . (extra-space . 0.5))
                        (staff-bar . (extra-space . 0.5))
                        (right-edge . (extra-space . 0.0))))
        (stencil . ,ly:bar-line::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (break-aligned-interface
                                font-interface
                                pure-from-neighbor-interface
                                signum-repetitionis-interface))))))

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
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-extent . ,slur::height)
        (meta . ((class . Spanner)
                 (interfaces . (bezier-curve-interface
                                outside-staff-interface
                                slur-interface))
                 (description . "A slur.  See also
@iref{PhrasingSlur}.")))))

    (SostenutoPedal
     . (
        (direction . ,RIGHT)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-shape . italic)
        (padding . 0.0) ;; padding relative to SostenutoPedalLineSpanner
        (parent-alignment-X . #f)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                piano-pedal-script-interface
                                self-alignment-interface
                                text-interface))
                 (description . "A sostenuto pedal mark.  See
also @iref{SostenutoPedalLineSpanner}, @iref{PianoPedalBracket},
@iref{SustainPedal}, and @iref{UnaCordaPedal}.")))))

    (SostenutoPedalLineSpanner
     . (
        (axes . (,Y))
        (direction . ,DOWN)
        (minimum-space . 1.0)
        (outside-staff-priority . 1000)
        (padding . 1.2)
        (side-axis . ,Y)
        (staff-padding . 1.0)
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-interface
                                piano-pedal-interface
                                side-position-interface))
                 (description . "An auxiliary grob providing a
baseline to align consecutive @iref{SostenutoPedal} grobs
vertically.")))))

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
                                spacing-spanner-interface))
                 (description . "An auxiliary grob to set all
horizontal spacing constraints across a score.  There is normally
one such grob for the whole score, but there can be several if
@code{\\newSpacingSection} is used.  See also
@iref{GraceSpacing}, @iref{NoteSpacing}, and
@iref{StaffSpacing}.")))))

    (SpanBar
     . (
        (allow-span-bar . #t)
        (bar-extent . ,axis-group-interface::height)
        (before-line-breaking . ,ly:span-bar::before-line-breaking)
        (break-align-anchor . ,ly:span-bar::calc-anchor)
        (break-align-symbol . staff-bar)
        (glyph-name . ,ly:span-bar::calc-glyph-name)
        (layer . 0)
        (non-musical . #t)
        (stencil . ,ly:span-bar::print)
        (X-extent . ,ly:span-bar::width)
        (Y-extent . (+inf.0 . -inf.0))
        (meta . ((class . Item)
                 (interfaces . (bar-line-interface
                                break-aligned-interface
                                font-interface
                                span-bar-interface))
                 (description . "A span bar, i.e., the parts of
a multi-staff bar line that are outside of staves.  See also
@iref{SpanBarStub}.")))))

    (SpanBarStub
     . (
        (X-extent . ,(grob::inherit-parent-property
                      X 'X-extent))
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height)
        ;; we want this to be ignored, so empty, but the extra spacing height
        ;; should preserve the span bar's presence for horizontal spacing
        (Y-extent . ,pure-from-neighbor-interface::height-if-pure)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (pure-from-neighbor-interface))
                 (description . "An auxiliary grob, acting like a
fake @iref{SpanBar} grob in contexts such as @iref{Lyrics} that
are crossed by a span bar, to keep span bars taking horizontal
space.")))))

    (StaffEllipsis ; indicates a cut made by skipTypesetting = ##t
     . (
        (break-align-symbol . staff-ellipsis)
        (break-visibility . ,all-visible)
        (layer . 1)
        (non-musical . #t)
        (space-alist . (
                        (ambitus . (extra-space . 1.0))
                        (breathing-sign . (extra-space . 1.0))
                        (custos . (extra-space . 1.0))
                        (key-signature . (extra-space . 1.0))
                        (time-signature . (extra-space . 1.0))
                        (signum-repetitionis . (extra-space . 1.0))
                        (staff-bar . (extra-space . 1.0))
                        (clef . (extra-space . 1.0))
                        (cue-clef . (extra-space . 1.0))
                        (cue-end-clef . (extra-space . 1.0))
                        (first-note . (extra-space . 1.0))
                        (right-edge . (fixed-space . 0))))
        (text . ,(make-line-markup (list
                                    (make-null-markup)
                                    (make-musicglyph-markup "dots.dot")
                                    (make-musicglyph-markup "dots.dot")
                                    (make-musicglyph-markup "dots.dot")
                                    (make-null-markup))))
        (stencil . ,staff-ellipsis::print)
        (whiteout . #t)
        (Y-extent . ,staff-ellipsis::calc-y-extent)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                font-interface
                                text-interface))
                 (description . "A visual marker (usually three
consecutive dots) to indicate that typesetting of music is
skipped.")))))

    (StaffGrouper
     . (
        (staff-staff-spacing . ((basic-distance . 9)
                                (minimum-distance . 7)
                                (padding . 1)
                                (stretchability . 5)))
        (staffgroup-staff-spacing . ((basic-distance . 10.5)
                                     (minimum-distance . 8)
                                     (padding . 1)
                                     (stretchability . 9)))
        (meta . ((class . Spanner)
                 (interfaces . (staff-grouper-interface))
                 (description . "An auxiliary grob to manage
vertical spacing of staff groups.  See also
@iref{VerticalAlignment} and @iref{VerticalAxisGroup}.")))))

    (StaffSpacing
     . (
        (non-musical . #t)
        (stem-spacing-correction . 0.4)
        (meta . ((class . Item)
                 (interfaces . (spacing-interface
                                staff-spacing-interface))
                 (description . "An auxiliary grob to handle
spacing within a staff.  See also @iref{NoteSpacing},
@iref{GraceSpacing}, and @iref{SpacingSpanner}.")))))

    (StaffHighlight
     . (
        (bound-prefatory-paddings . (0.5 . 0.5))
        (color . ,(grob::calc-property-by-copy 'color))
        (stencil . ,staff-highlight::print)
        (layer . -1)
        (shorten-pair . (0 . 0))
        (X-extent . ,staff-highlight::width)
        (Y-extent . ,staff-highlight::height)
        (meta . ((class . Spanner)
                 (interfaces . (staff-highlight-interface))
                 (description . "A colored span to highlight a music passage.")))))

    (StaffSymbol
     . (
        (break-align-symbols . (staff-bar break-alignment))
        (layer . 0)
        (ledger-line-thickness . (1.0 . 0.1))
        (line-count . 5)
        (stencil . ,ly:staff-symbol::print)
        (widened-extent . ,staff-symbol::calc-widened-extent)
        (Y-extent . ,(ly:make-unpure-pure-container ly:staff-symbol::height))
        (meta . ((class . Spanner)
                 (interfaces . (staff-symbol-interface))
                 (description . "A staff symbol, usually
five horizontal lines.")))))

    (StanzaNumber
     . (
        (direction . ,LEFT)
        (font-series . bold)
        (padding . 1.0)
        (side-axis . ,X)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                side-position-interface
                                stanza-number-interface
                                text-interface))
                 (description . "A stanza number (or markup) for
lyrics.")))))

    (Stem
     . (
        (beamlet-default-length . (1.1 . 1.1))
        (beamlet-max-length-proportion . (0.75 . 0.75))
        (cross-staff . ,ly:stem::calc-cross-staff)
        (default-direction . ,ly:stem::calc-default-direction)
        (details
         . (
            ;; 3.5 (or 3 measured from note head) is standard length
            ;; 32nd, 64th, ..., 1024th flagged stems should be longer
            (lengths . (3.5 3.5 3.5 4.25 5.0 6.0 7.0 8.0 9.0))

            ;; FIXME.  3.5 yields too long beams (according to Ross and
            ;; looking at Baerenreiter examples) for a number of common
            ;; boundary cases.  Subtracting half a beam thickness fixes
            ;; this, but the bug may well be somewhere else.

            ;; FIXME this should come from 'lengths
            (beamed-lengths . (3.26 3.5 3.6))

            ;; The 'normal' minima
            (beamed-minimum-free-lengths . (1.83 1.5 1.25))
            ;;(beamed-minimum-free-lengths . (2.0 1.83 1.25))

            ;; The 'extreme case' minima
            (beamed-extreme-minimum-free-lengths . (2.0 1.25))

            ;; Stems in unnatural (forced) direction should be shortened by
            ;; one staff space, according to [Roush & Gourlay].
            ;; Flagged stems we shorten only half a staff space.
            (stem-shorten . (1.0 0.5 0.25))

            ))

        ;; We use the normal minima as minimum for the ideal lengths,
        ;; and the extreme minima as abolute minimum length.

        (direction . ,ly:stem::calc-direction)
        (double-stem-separation . 0.5)
        (duration-log . ,stem::calc-duration-log)
        (length . ,(ly:make-unpure-pure-container ly:stem::calc-length ly:stem::pure-calc-length))
        (neutral-direction . ,DOWN)
        (note-collision-threshold . 1)
        (positioning-done . ,ly:stem::calc-positioning-done)
        (stem-info . ,ly:stem::calc-stem-info)
        (stem-begin-position . ,(ly:make-unpure-pure-container ly:stem::calc-stem-begin-position ly:stem::pure-calc-stem-begin-position))
        (stencil . ,ly:stem::print)
        (thickness . 1.3)
        (X-extent . ,ly:stem::width)
        (X-offset . ,ly:stem::offset-callback)
        (Y-extent . ,(ly:make-unpure-pure-container ly:stem::height ly:stem::pure-height))
        (Y-offset . ,staff-symbol-referencer::callback)
        (meta . ((class . Item)
                 (interfaces . (stem-interface))
                 (description . "A stem.  See also
@iref{StemStub}.")))))

    (StemStub
     . (
        (X-extent . ,stem-stub::width)
        (extra-spacing-height . ,stem-stub::extra-spacing-height)
        (Y-extent . ,(ly:make-unpure-pure-container #f stem-stub::pure-height))
        (meta . ((class . Item)
                 (interfaces . ())
                 (description . "An auxiliary grob that prevents
cross-staff @iref{Stem} grobs from colliding with
articulations.")))))

    (StemTremolo
     . (
        (beam-thickness . 0.48) ; staff-space
        (beam-width . ,ly:stem-tremolo::calc-width) ; staff-space
        (cross-staff . ,ly:stem-tremolo::calc-cross-staff)
        (direction . ,ly:stem-tremolo::calc-direction)
        (parent-alignment-X . ,CENTER)
        (slope . ,ly:stem-tremolo::calc-slope)
        (stencil . ,ly:stem-tremolo::print)
        (shape . ,ly:stem-tremolo::calc-shape)
        (X-extent . ,ly:stem-tremolo::width)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,(grob::unpure-Y-extent-from-stencil ly:stem-tremolo::pure-height))
        (Y-offset . ,(ly:make-unpure-pure-container ly:stem-tremolo::calc-y-offset ly:stem-tremolo::pure-calc-y-offset))
        (meta . ((class . Item)
                 (interfaces . (self-alignment-interface
                                stem-tremolo-interface))
                 (description . "A stem tremolo.")))))

    (StringNumber
     . (
        (add-stem-support . ,only-if-beamed)
        (avoid-slur . around)
        (cross-staff . ,script-or-side-position-cross-staff)
        (font-encoding . fetaText)
        (font-features . ("cv47"))
        (font-size . -5)                ; don't overlap when next to heads.
        (number-type . arabic)
        (padding . 0.5)
        (parent-alignment-X . ,CENTER)
        (script-priority . 100)
        (self-alignment-X . ,CENTER)
        (self-alignment-Y . ,CENTER)
        (staff-padding . 0.5)
        (stencil . ,print-circled-text-callback)
        (text . ,string-number::calc-text)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                self-alignment-interface
                                outside-staff-interface
                                side-position-interface
                                number-interface
                                string-number-interface
                                text-interface
                                text-script-interface))
                 (description . "A markup (by default a digit in
a circle) to name a string.")))))

    (StrokeFinger
     . (
        (add-stem-support . ,only-if-beamed)
        (digit-names . #("p" "i" "m" "a" "x"))
        (font-shape . italic)
        (font-size . -4)                ; don't overlap when next to heads.
        (padding . 0.5)
        (parent-alignment-X . ,CENTER)
        (script-priority . 100)
        (self-alignment-X . ,CENTER)
        (self-alignment-Y . ,CENTER)
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (text . ,stroke-finger::calc-text)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                stroke-finger-interface
                                text-interface
                                text-script-interface))
                 (description . "A markup (usually a lowercase
letter) to indicate right-hand fingering.  See also
@iref{Fingering}.")))))

    (SustainPedal
     . (
        (extra-spacing-width . (+inf.0 . -inf.0))
        (padding . 0.0)  ;; padding relative to SustainPedalLineSpanner
        (parent-alignment-X . #f)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:sustain-pedal::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                piano-pedal-interface
                                piano-pedal-script-interface
                                self-alignment-interface
                                text-interface))
                 (description . "A sustain pedal mark.  See also
@iref{SustainPedalLineSpanner}, @iref{PianoPedalBracket},
@iref{SostenutoPedal}, and @iref{UnaCordaPedal}.")))))

    (SustainPedalLineSpanner
     . (
        (axes . (,Y))
        (direction . ,DOWN)
        (minimum-space . 1.0)
        (outside-staff-priority . 1000)
        (padding . 1.2)
        (side-axis . ,Y)
        (staff-padding . 1.2)
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-interface
                                piano-pedal-interface
                                side-position-interface))
                 (description . "An auxiliary grob providing a
baseline to align consecutive @iref{SustainPedal} grobs
vertically.")))))

    (System
     . (
        (adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
        (axes . (,X ,Y))
        (outside-staff-placement-directive . left-to-right-polite)
        (show-vertical-skylines . ,grob::show-skylines-if-debug-skylines-set)
        (skyline-horizontal-padding . 1.0)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,(ly:make-unpure-pure-container ly:system::height ly:system::calc-pure-height))
        (meta . ((class . System)
                 (object-callbacks . ((footnotes-before-line-breaking . ,ly:system::footnotes-before-line-breaking)
                                      (footnotes-after-line-breaking . ,ly:system::footnotes-after-line-breaking)
                                      (pure-relevant-grobs . ,ly:system::calc-pure-relevant-grobs)
                                      (pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (vertical-skyline-elements . ,ly:system::vertical-skyline-elements)
                                      (vertical-alignment . ,ly:system::get-vertical-alignment)))
                 (interfaces . (axis-group-interface
                                outside-staff-axis-group-interface))
                 (description . "The top-level grob of a score.
All other grobs are descendants of it.")))))

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
        (meta . ((class . Spanner)
                 (interfaces . (side-position-interface
                                system-start-delimiter-interface))
                 (description . "A bar line as a system start
delimiter.")))))

    (SystemStartBrace
     . (
        (collapse-height . 5.0)
        (direction . ,LEFT)
        (font-encoding . fetaBraces)
        (padding . 0.3)
        (stencil . ,ly:system-start-delimiter::print)
        (style . brace)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                side-position-interface
                                system-start-delimiter-interface))
                 (description . "A brace as a system start
delimiter.")))))

    (SystemStartBracket
     . (
        (collapse-height . 5.0)
        (direction . ,LEFT)
        (padding . 0.8)
        (stencil . ,ly:system-start-delimiter::print)
        (style . bracket)
        (thickness . 0.45)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                side-position-interface
                                system-start-delimiter-interface))
                 (description . "A bracket as a system start
delimiter.")))))

    (SystemStartSquare
     . (
        (collapse-height . 5.0)
        (direction . ,LEFT)
        (stencil . ,ly:system-start-delimiter::print)
        (style . line-bracket)
        (thickness . 1.0)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                side-position-interface
                                system-start-delimiter-interface))
                 (description . "A rectangle-like bracket as a
start delimiter.")))))


    (TabNoteHead
     . (
        (bend-me . ())
        (details . ((cautionary-properties . ((angularity . 0.4)
                                              (half-thickness . 0.075)
                                              (padding . 0)
                                              (procedure . ,parenthesize-stencil)
                                              (width . 0.25)))
                    (head-offset . 3/5)
                    (harmonic-properties . ((angularity . 2)
                                            (half-thickness . 0.075)
                                            (padding . 0)
                                            (procedure . ,parenthesize-stencil)
                                            (width . 0.25)))
                    (repeat-tied-properties . ((note-head-visible . #t)
                                               (parenthesize . #t)))
                    (tied-properties . ((parenthesize . #t)))))

        (direction . ,CENTER)
        (duration-log . ,note-head::calc-duration-log)
        (font-series . bold)
        (font-size . -2)
        (parenthesis-friends . (dot))
        (stem-attachment . ,ly:note-head::calc-tab-stem-attachment)
        (stencil . ,tab-note-head::print)
        (whiteout . #t)
        (X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces  . (bend-interface
                                 font-interface
                                 note-head-interface
                                 rhythmic-grob-interface
                                 rhythmic-head-interface
                                 staff-symbol-referencer-interface
                                 tab-note-head-interface
                                 text-interface))
                 (description . "A @q{note head} (usually a
digit) in a tablature.  See also @iref{NoteHead}.")))))

    (TextMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (baseline-skip . 2)
        (break-align-symbols . (staff-bar key-signature clef))
        (break-visibility . ,text-mark-interface::calc-break-visibility)
        (direction . ,UP)
        ;; See \markLengthOn
        (extra-spacing-width . (+inf.0 . -inf.0))
        ;; slightly bigger than TextScript, smaller than SectionLabel
        (font-size . 0.5)
        (non-musical . #t)
        (outside-staff-horizontal-padding . 0.2)
        ;; More than CenteredBarNumberLineSpanner but less than MetronomeMark.
        ;; This is a guess; there isn't a single use case for text marks, so it
        ;; is expected that the user might have to tweak outside-staff-priority
        ;; if needed.
        (outside-staff-priority . 1250)
        (padding . 0.8)
        (self-alignment-X . ,text-mark-interface::calc-self-alignment-X)
        (stencil . ,ly:text-interface::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                break-alignable-interface
                                font-interface
                                mark-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface
                                text-mark-interface))
                 (description . "An arbitrary textual mark.
See also @iref{SectionLabel} and @iref{JumpScript} for grobs with a
more specific intent.")))))

    (TextScript
     . (
        (avoid-slur . around)
        (cross-staff . #f)
        (direction . ,DOWN)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 450)

        ;; sync with Fingering ?
        (padding . 0.3)

        (parent-alignment-X . #f)
        (script-priority . 200)
        ;; self-alignment cannot be LEFT because of fingering diagrams.
        (self-alignment-X . #f)
        (side-axis . ,Y)
        (slur-padding . 0.5)
        (staff-padding . 0.5)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-align-on-main-noteheads . #t)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Item)
                 (interfaces . (accidental-switch-interface
                                font-interface
                                instrument-specific-markup-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface
                                text-script-interface))
                 (description . "A markup attached to a grob like
a note head.  See also @iref{MultiMeasureRestText}.")))))

    (TextSpanner
     . (
        (bound-details . ((left . ((padding . 0.25)
                                   (attach-dir . ,LEFT)
                                   ))
                          (left-broken . ((attach-dir . ,RIGHT)))
                          (right . ((padding . 0.25)
                                    ))
                          ))
        (dash-fraction . 0.2)
        (dash-period . 3.0)
        (direction . ,UP)
        (font-shape . italic)
        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
        (outside-staff-priority . 350)
        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (side-axis . ,Y)
        (staff-padding . 0.8)
        (stencil . ,ly:line-spanner::print)
        (style . dashed-line)
        (Y-offset . ,side-position-interface::y-aligned-side)

        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                outside-staff-interface
                                side-position-interface))
                 (description . "Text like @q{rit}, usually
followed by a (dashed) line.  See also
@iref{DynamicTextSpanner}.")))))

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
        (line-thickness . 0.8)
        (neutral-direction . ,UP)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:tie::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (thickness . 1.2)
        (meta . ((class . Spanner)
                 (interfaces . (bezier-curve-interface
                                tie-interface))
                 (description . "A tie.  See also
@iref{TieColumn}, @iref{LaissezVibrerTie}, and
@iref{RepeatTie}.")))))

    (TieColumn
     . (
        (before-line-breaking . ,ly:tie-column::before-line-breaking)
        (positioning-done . ,ly:tie-column::calc-positioning-done)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Spanner)
                 (interfaces . (tie-column-interface))
                 (description . "An auxiliary grob to determine
direction and shape of stacked @iref{Tie} grobs.")))))

    (TimeSignature
     . (
        (avoid-slur . inside)
        (break-align-anchor
         . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-symbol . time-signature)
        (break-align-anchor-alignment . ,LEFT)
        (break-visibility . ,all-visible)
        (extra-spacing-height . ,pure-from-neighbor-interface::extra-spacing-height-including-staff)
        (extra-spacing-width . (0.0 . 0.8))
        (non-musical . #t)
        (space-alist . (
                        (ambitus . (extra-space . 1.0))
                        (cue-clef . (extra-space . 1.5))
                        (first-note . (fixed-space . 2.0))
                        (right-edge . (extra-space . 0.5))
                        (signum-repetitionis . (extra-space . 1.0))
                        (staff-bar . (extra-space . 1.0))))
        (stencil . ,ly:time-signature::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (style . C)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:pure-from-neighbor-interface::calc-pure-relevant-grobs)))
                 (interfaces . (break-aligned-interface
                                font-interface
                                pure-from-neighbor-interface
                                time-signature-interface))
                 (description . "A time signature.")))))

    (TrillPitchAccidental
     . (
        (direction . ,LEFT)
        (font-size . -4)
        (glyph-name . ,accidental-interface::calc-glyph-name)
        (padding . 0.2)
        (side-axis . ,X)
        (stencil . ,ly:accidental-interface::print)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                accidental-switch-interface
                                font-interface
                                inline-accidental-interface
                                side-position-interface
                                trill-pitch-accidental-interface))
                 (description . "The accidental of a pitched
trill.  See also @iref{TrillPitchGroup}.")))))

    (TrillPitchGroup
     . (
        (axes . (,X))
        (direction . ,RIGHT)
        ;; minimum shift to the right, in case the parent note has no stem
        (minimum-space . 2.5)
        (horizon-padding . 0.1) ; to avoid interleaving with augmentation dots
        (padding . 0.3)
        (side-axis . ,X)
        (X-extent . ,ly:axis-group-interface::width)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (Y-extent . ,(ly:make-unpure-pure-container
                      ly:axis-group-interface::height
                      trill-pitch-group::pure-height))
        (meta . ((class . Item)
                 (interfaces . (axis-group-interface
                                side-position-interface))
                 (description . "An auxiliary grob to construct
a pitched trill, aligning @iref{TrillPitchAccidental},
@iref{TrillPitchParentheses}, and @iref{TrillPitchHead}
horizontally.  See also @iref{TrillSpanner}.")))))

    (TrillPitchHead
     . (
        (duration-log . 2)
        (font-size . -4)
        (parenthesis-friends . (accidental-grob))
        (stencil . ,ly:note-head::print)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                ledgered-interface
                                pitched-trill-interface
                                rhythmic-head-interface
                                staff-symbol-referencer-interface))
                 (description . "The note head of a pitched
trill.  See also @iref{TrillPitchGroup}.")))))

    (TrillPitchParentheses
     . (
        ;; This is different from Parentheses, which has -6 plus the font size
        ;; of its host.
        (font-size . -4)
        ;; This default comes from a time when parentheses were baked into
        ;; TrillPitchGroup and the padding defined both the inner padding
        ;; of the parentheses and the padding of the whole group from the
        ;; main note column.  It could be revisited (Parentheses has 0.2).
        (padding . 0.3)
        (stencil . ,parentheses-interface::print)
        (stencils . ,parentheses-interface::calc-parenthesis-stencils)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                parentheses-interface
                                pitched-trill-interface))
                 (description . "The parentheses of a pitched
trill.  See also @iref{TrillPitchGroup}.")))))

    (TrillSpanner
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (bound-details . ((left . (
                                   ;; Need true X extent for chained trills not to overlap.
                                   ;; The trill glyph has a loop on its left, which sticks
                                   ;; out of its bounding box.
                                   (text . ,(make-with-true-dimension-markup
                                             X
                                             (make-musicglyph-markup "scripts.trill")))
                                   (stencil-offset . (0 . -1))
                                   (attach-dir . ,CENTER)
                                   ))
                          (left-broken . ((end-on-note . #t)))
                          (right . ((adjust-on-neighbor . #t)
                                    (attach-dir . ,LEFT)
                                    (end-on-accidental . #t)))
                          ))
        (direction . ,UP)
        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
        (outside-staff-priority . 50)
        (padding . 0.5)
        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (staff-padding . 1.0)
        (stencil . ,ly:line-spanner::print)
        (style . trill)
        (to-barline . #t)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                outside-staff-interface
                                side-position-interface
                                trill-spanner-interface))
                 (description . "A continued trill with a wiggly
line (created with @code{\\startTrillSpan}, not with
@code{\\trill}).  See also @iref{TrillPitchGroup}.")))))

    (TupletBracket
     . (
        (avoid-scripts . #t)
        (connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
        (cross-staff . ,ly:tuplet-bracket::calc-cross-staff)
        (direction  . ,ly:tuplet-bracket::calc-direction)
        (edge-height . (0.7 . 0.7))
        (full-length-to-extent . #t)
        (visible-over-note-heads . #f)
        (padding . 1.1)
        (positions . ,ly:tuplet-bracket::calc-positions)
        (shorten-pair . (-0.2 . -0.2))
        (staff-padding . 0.25)
        (stencil . ,ly:tuplet-bracket::print)
        (thickness . 1.6)
        (tuplet-slur . #f)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-positions . ,ly:tuplet-bracket::calc-x-positions)

        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                outside-staff-interface
                                tuplet-bracket-interface))
                 (description . "A tuplet bracket.  See also
@iref{TupletNumber}.")))))

    (TupletNumber
     . (
        (avoid-slur . inside)
        (cross-staff . ,ly:tuplet-number::calc-cross-staff)
        (direction . ,tuplet-number::calc-direction)
        (font-shape . italic)
        (font-size . -2)
        (knee-to-beam . #t)
        (stencil . ,ly:tuplet-number::print)
        (text . ,tuplet-number::calc-denominator-text)
        (X-offset . ,ly:tuplet-number::calc-x-offset)
        (Y-offset . ,ly:tuplet-number::calc-y-offset)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                outside-staff-interface
                                text-interface
                                tuplet-number-interface))
                 (description . "A tuplet number.  See also
@iref{TupletBracket}.")))))


    (UnaCordaPedal
     . (
        (direction . ,RIGHT)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (font-shape . italic)
        (padding . 0.0)  ;; padding relative to UnaCordaPedalLineSpanner
        (parent-alignment-X . #f)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:text-interface::print)
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                piano-pedal-script-interface
                                self-alignment-interface
                                text-interface))
                 (description . "An una corda pedal mark.  See
also @iref{UnaCordaPedalLineSpanner}, @iref{SostenutoPedal},
@iref{SustainPedal}, and @iref{PianoPedalBracket}.")))))

    (UnaCordaPedalLineSpanner
     . (
        (axes . (,Y))
        (direction . ,DOWN)
        (minimum-space . 1.0)
        (outside-staff-priority . 1000)
        (padding . 1.2)
        (side-axis . ,Y)
        (staff-padding . 1.2)
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-interface
                                piano-pedal-interface
                                side-position-interface))
                 (description . "An auxiliary grob providing a
baseline to align consecutive @iref{UnaCordaPedal} grobs
vertically.")))))


    (VaticanaLigature
     . (
        (flexa-width . 2.0)
        (stencil . ,ly:vaticana-ligature::print)
        (thickness . 0.6)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                vaticana-ligature-interface))
                 (description . "A grob to display a melisma
(ligature) as used in Gregorian chant.  See also
@iref{KievanLigature}, @iref{MensuralLigature}, and
@iref{LigatureBracket}.")))))

    (VerticalAlignment
     . (
        (axes . (,Y))
        (positioning-done . ,ly:align-interface::align-to-ideal-distances)
        (stacking-dir . -1)
        (vertical-skylines . ,ly:axis-group-interface::combine-skylines)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (meta . ((class . Spanner)
                 (object-callbacks . ((Y-common . ,ly:axis-group-interface::calc-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)
                                      (pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)))
                 (interfaces . (align-interface
                                axis-group-interface))
                 (description . "A top-level auxiliary grob to
stack groups (staves, lyrics lines, etc.).  See also
@iref{StaffGrouper} and @iref{VerticalAxisGroup}.")))))

    (VerticalAxisGroup
     . (
        (adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
        (axes . (,Y))
        (default-staff-staff-spacing . ((basic-distance . 9)
                                        (minimum-distance . 8)
                                        (padding . 1)))
        (nonstaff-unrelatedstaff-spacing . ((padding . 0.5)))
        (outside-staff-placement-directive . left-to-right-polite)
        (staff-staff-spacing . ,(ly:make-unpure-pure-container ly:axis-group-interface::calc-staff-staff-spacing ly:axis-group-interface::calc-pure-staff-staff-spacing))
        (show-vertical-skylines . ,grob::show-skylines-if-debug-skylines-set)
        (skyline-horizontal-padding . 0.1)
        (vertical-skylines . ,ly:hara-kiri-group-spanner::calc-skylines)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,(ly:make-unpure-pure-container ly:hara-kiri-group-spanner::y-extent ly:hara-kiri-group-spanner::pure-height))
        (Y-offset . ,ly:hara-kiri-group-spanner::force-hara-kiri-callback)
        (meta . ((class . Spanner)
                 (object-callbacks . (
                                      (X-common . ,ly:axis-group-interface::calc-x-common)
                                      (pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))

                 (interfaces . (axis-group-interface
                                hara-kiri-group-spanner-interface
                                outside-staff-axis-group-interface))
                 (description . "An auxiliary grob to group
everything contained in a context like @iref{Staff},
@iref{Lyrics}, @iref{Dynamics}, etc.  See also @iref{StaffGrouper}
and @iref{VerticalAlignment}.")))))

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
        (cross-staff . #t)
        (gap . 0.5)
        (left-bound-info . ,ly:line-spanner::calc-left-bound-info)
        (normalized-endpoints . ,ly:spanner::calc-normalized-endpoints)
        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                line-spanner-interface))
                 (description . "A line to indicate staff changes
of a voice.")))))

    (VoltaBracket
     . (
        (baseline-skip . 1.7)
        (direction . ,UP)
        (edge-height . (2.0 . 2.0)) ;; staff-space;
        (font-encoding . fetaText)
        (font-features . ("cv47" "ss01"))
        (font-size . -4)
        (shorten-pair . ,ly:volta-bracket::calc-shorten-pair)
        (stencil . ,ly:volta-bracket-interface::print)
        (thickness . 1.6) ;; line-thickness
        (word-space . 0.6)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-extent . ,(grob::unpure-Y-extent-from-stencil volta-bracket-interface::pure-height))
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                horizontal-bracket-interface
                                line-interface
                                side-position-interface
                                text-interface
                                volta-bracket-interface
                                volta-interface))
                 (description . "A volta bracket.  See also
@iref{VoltaBracketSpanner}.")))))

    (VoltaBracketSpanner
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (axes . (,Y))
        (direction . ,UP)
        (outside-staff-priority . 600)
        (padding . 1)
        (side-axis . ,Y)
        (vertical-skylines . ,grob::always-vertical-skylines-from-element-stencils)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-interface
                                side-position-interface
                                volta-interface))
                 (description . "An auxiliary grob providing a
baseline to align consecutive @iref{VoltaBracket} grobs
vertically.")))))

    (VowelTransition
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (arrow-length . 0.5)
        (arrow-width . 0.5)
        (bound-details . ((left . ((padding . 0.14)
                                   (attach-dir . ,RIGHT)
                                   ))
                          (right-broken . ((padding . 0)))
                          (left-broken . ((padding . 0)))
                          (right . ((padding . 0.14)
                                    (attach-dir . ,LEFT)
                                    (arrow . #t)
                                    ))))
        (left-bound-info . ,ly:horizontal-line-spanner::calc-left-bound-info)
        (minimum-length . 1.0)
        (right-bound-info . ,ly:horizontal-line-spanner::calc-right-bound-info)
        (springs-and-rods . ,ly:vowel-transition::set-spacing-rods)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-offset . 0.5)
        (meta . ((class . Spanner)
                 (interfaces . (horizontal-line-spanner-interface
                                line-interface
                                line-spanner-interface
                                lyric-interface))
                 (description . "A vowel transition in
lyrics.  See also @iref{LyricHyphen}.")))))

    ))

(define (completize-grob-entry x)
  "Transplant assoc key into @code{meta.name} of @var{X}.
Add @code{grob-interface}.  If @code{meta.class} is provided,
turn it into a one-element list in @code{meta.classes}.
"
  (let* ((name-sym  (car x))
         ;; Make (shallow) copies of the list and its items because we modify
         ;; them below.
         (grob-entry (map list-copy (cdr x)))
         (meta-entry (map list-copy (assoc-get 'meta grob-entry)))
         (class-entry
          (assoc-get 'class meta-entry #f))
         (ifaces-entry
          (assoc-get 'interfaces meta-entry)))

    (if class-entry
        (set! meta-entry
              (assoc-set! meta-entry
                          'classes
                          (list class-entry))))

    (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
    (set! ifaces-entry (cons 'grob-interface ifaces-entry))

    (set! meta-entry (assoc-set! meta-entry 'name name-sym))
    (set! meta-entry (assoc-set! meta-entry 'interfaces ifaces-entry))
    (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))

    ;; make sure that \property Foo.Bar =\turnOff doesn't complain
    (set-object-property! name-sym 'translation-type? ly:grob-properties?)
    (set-object-property! name-sym 'is-grob? #t)

    (cons name-sym grob-entry)))

(define-session-public all-grob-descriptions
  (sort (map completize-grob-entry all-grob-descriptions-data) alist<?))
