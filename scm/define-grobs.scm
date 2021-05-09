;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
;;;; WARNING: don't use anonymous functions for initialization.

;; TODO: junk the meta field in favor of something more compact?


(define all-grob-descriptions-data
  `(
    (Accidental
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (avoid-slur . inside)
        (extra-spacing-width . (-0.2 . 0.0))
        (glyph-name . ,accidental-interface::glyph-name)
        (glyph-name-alist . ,standard-alteration-glyph-name-alist)
        (stencil . ,ly:accidental-interface::print)
        (horizontal-skylines . ,(ly:make-unpure-pure-container ly:accidental-interface::horizontal-skylines))
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                inline-accidental-interface
                                font-interface))))))

    (AccidentalCautionary
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (avoid-slur . inside)
        (glyph-name-alist . ,standard-alteration-glyph-name-alist)
        (parenthesized . #t)
        (stencil . ,ly:accidental-interface::print)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                inline-accidental-interface
                                font-interface))))))

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
                 (interfaces . (accidental-placement-interface))))))

    (AccidentalSuggestion
     . (
        (after-line-breaking . ,ly:accidental-interface::remove-tied)
        (alteration . ,accidental-interface::calc-alteration)
        (direction . ,UP)
        (font-size . -2)
        (glyph-name-alist . ,standard-alteration-glyph-name-alist)
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
                                font-interface
                                outside-staff-interface
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
                        (cue-end-clef . (extra-space . 0.5))
                        (clef . (extra-space . 1.15))
                        (cue-clef . (extra-space . 0.5))
                        (key-signature . (extra-space . 1.15))
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
                                break-aligned-interface))))))

    (AmbitusAccidental
     . (
        (direction . ,LEFT)
        (glyph-name-alist . ,standard-alteration-glyph-name-alist)
        (padding . 0.5)
        (side-axis . ,X)
        (stencil . ,ly:accidental-interface::print)
        (X-offset . ,ly:grob::x-parent-positioning)
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                break-aligned-interface
                                font-interface
                                side-position-interface))))))

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
                                font-interface))))))

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
                                rhythmic-head-interface
                                staff-symbol-referencer-interface))))))

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
                                staff-symbol-referencer-interface))))))

    (BalloonTextItem
     . (
        (annotation-balloon . #t)
        (annotation-line . #t)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (stencil . ,ly:balloon-interface::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (balloon-interface
                                font-interface
                                text-interface))))))

    (BalloonTextSpanner
     . (
        (annotation-balloon . #t)
        (annotation-line . #t)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (stencil . ,ly:balloon-interface::print-spanner)
        (text . ,(grob::calc-property-by-copy 'text))
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (Y-extent . ,balloon::height)
        (meta . ((class . Spanner)
                 (interfaces . (balloon-interface
                                font-interface
                                text-interface))))))

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
        (glyph-name . ,bar-line::calc-glyph-name)

        ;;
        ;; Ross. page 151 lists other values, we opt for a leaner look
        ;;
        ;; TODO:
        ;; kern should scale with line-thickness too.
        (kern . 3.0)
        (segno-kern . 3.0)
        (hair-thickness . 1.9)
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
                                pure-from-neighbor-interface))))))

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
        (self-alignment-X . ,RIGHT)
        (side-axis . ,Y)
        (stencil . ,ly:text-interface::print)
        (X-offset . ,self-alignment-interface::self-aligned-on-breakable)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta .
              ((class . Item)
               (interfaces . (break-alignable-interface
                              font-interface
                              outside-staff-interface
                              self-alignment-interface
                              side-position-interface
                              text-interface))))))

    (BassFigure
     . (
        (stencil . ,ly:text-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
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
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (align-interface
                                axis-group-interface
                                bass-figure-alignment-interface))))))

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
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (meta . ((class . Spanner)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                outside-staff-axis-group-interface))))))


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
        (break-align-anchor-alignment . ,ly:break-aligned-interface::calc-joint-anchor-alignment)
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
                               cue-end-clef
                               ambitus
                               breathing-sign
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
                               cue-end-clef
                               ambitus
                               breathing-sign
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
                               ambitus
                               breathing-sign
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
                        (cue-clef . (minimum-space . 2.0))
                        (cue-end-clef . (minimum-space . 2.0))
                        (first-note . (fixed-space . 1.0)) ;huh?
                        (right-edge . (extra-space . 0.1))))
        (stencil . ,ly:text-interface::print)
        (text . ,(make-musicglyph-markup "scripts.rcomma"))
        (Y-offset . ,(ly:make-unpure-pure-container ly:breathing-sign::offset-callback))
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                breathing-sign-interface
                                font-interface
                                outside-staff-interface
                                text-interface))))))

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
                 (interfaces . (chord-name-interface
                                font-interface
                                outside-staff-interface
                                rhythmic-grob-interface
                                text-interface))))))

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
                                staff-symbol-referencer-interface))))))

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
                                text-interface))))))

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
                 (interfaces . (font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface
                                text-script-interface))))))

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
        (space-alist . ((staff-bar . (minimum-space . 2.7))
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
                                staff-symbol-referencer-interface))))))

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
                                staff-symbol-referencer-interface))))))

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
                                 staff-symbol-referencer-interface))))))

    (DotColumn
     . (
        (axes . (,X))
        (chord-dots-limit . 3)
        (direction . ,RIGHT)
        (positioning-done . ,ly:dot-column::calc-positioning-done)
        (X-extent . ,ly:axis-group-interface::width)
        (meta . ((class . Item)
                 (interfaces . (axis-group-interface
                                dot-column-interface))))))

    (Dots
     . (
        (avoid-slur . inside)
        (dot-count . ,dots::calc-dot-count)
        (staff-position . ,dots::calc-staff-position)
        (stencil . ,ly:dots::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (extra-spacing-height . (-0.5 . 0.5))
        (extra-spacing-width . (0.0 . 0.2))
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
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (break-aligned-interface
                                font-interface
                                percent-repeat-interface
                                percent-repeat-item-interface))))))

    (DoublePercentRepeatCounter
     . (
        (direction . ,UP)
        (font-encoding . fetaText)
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
                                percent-repeat-interface
                                percent-repeat-item-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))))))

    (DoubleRepeatSlash
     . (
        (dot-negative-kern . 0.75)
        (font-encoding . fetaMusic)
        (slash-negative-kern . 1.6)
        (slope . 1.0)
        (stencil . ,ly:percent-repeat-item-interface::beat-slash)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                outside-staff-interface
                                percent-repeat-interface
                                percent-repeat-item-interface
                                rhythmic-grob-interface))))))

    (DurationLine
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (arrow-width . 1.5)
        (arrow-length . 2)
        (bound-details
         .
         ((right . ((end-on-accidental . #t)
                    (end-on-arpeggio . #t)
                    (padding . 0.4)
                    ;; possible values for endstyle: arrow, hook
                    (end-style . #f)))
          (right-broken . ((padding . 0.4)
                           (end-style . #f)))
          (left-broken . ((padding . 0.4)))
          (left . ((padding . -0.3)
                   (start-at-dot . #f)))))
        (breakable . #t)
        ;; TODO needed/wished?
        (cross-staff . ,ly:line-spanner::calc-cross-staff)
        (details
         .
         ((hook-height . 0.34)
          ;; Unless set by the user, grob's thickness is taken as default
          (hook-thickness . #f)
          (hook-direction . ,UP)))
        (minimum-length . 2)
        (minimum-length-after-break . 6)
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
                 (interfaces . (spanner-interface
                                line-interface
                                line-spanner-interface
                                duration-line-interface
                                font-interface
                                unbreakable-spanner-interface))))))

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
                                side-position-interface))))))

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
                                text-interface))))))

    (DynamicTextSpanner
     . (
        (before-line-breaking . ,dynamic-text-spanner::before-line-breaking)
        (bound-details . ((right . ((attach-dir .  ,LEFT)
                                    (Y . 0)
                                    (padding . 0.75)
                                    ))
                          (right-broken . ((attach-dir .  ,RIGHT)
                                           (padding . 0.0)
                                           ))

                          (left . ((attach-dir .  ,LEFT)
                                   (Y . 0)
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

        (left-bound-info . ,ly:line-spanner::calc-left-bound-info-and-text)

        (minimum-length . 2.0)
        ;; make sure the spanner doesn't get too close to notes
        (minimum-Y-extent . (-1 . 1))

        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (skyline-horizontal-padding . 0.2)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:line-spanner::print)
        (style . dashed-line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (meta . ((class . Spanner)
                 (interfaces . (dynamic-interface
                                dynamic-text-spanner-interface
                                font-interface
                                line-interface
                                line-spanner-interface
                                ;; for now, LilyPond never will typeset
                                ;; these without a DynamicLineSpanner
                                ;; as their controlling element
                                ;; so, they do not need the
                                ;; outside-staff-interface
                                spanner-interface
                                text-interface))))))


    (Episema
     . (
        (bound-details . ((left . ((Y . 0)
                                   (padding . 0)
                                   (attach-dir . ,LEFT)
                                   ))
                          (right . ((Y . 0)
                                    (padding . 0)
                                    (attach-dir . ,RIGHT)
                                    ))
                          ))
        (direction . ,UP)
        (left-bound-info . ,ly:line-spanner::calc-left-bound-info)
        (quantize-position . #t)
        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (side-axis . ,Y)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (episema-interface
                                font-interface
                                line-interface
                                line-spanner-interface
                                side-position-interface))))))


    (Fingering
     . (

        ;; sync with TextScript (?)
        (add-stem-support . ,only-if-beamed)
        (avoid-slur . around)
        (cross-staff . ,script-or-side-position-cross-staff)
        (direction . ,ly:script-interface::calc-direction)
        (font-encoding . fetaText)
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
                                text-script-interface))))))

    (FingeringColumn
     . (
        (padding . 0.2)
        (positioning-done . ,ly:fingering-column::calc-positioning-done)
        (snap-radius . 0.3)
        (meta . ((class . Item)
                 (interfaces . (fingering-column-interface))))))

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
                                font-interface))))))

    (FootnoteItem
     . (
        (annotation-balloon . #f)
        (annotation-line . #t)
        (automatically-numbered . ,(grob::calc-property-by-copy 'automatically-numbered))
        (break-visibility . ,(grob::inherit-parent-property
                              X 'break-visibility))
        (footnote . #t)
        (footnote-text . ,(grob::calc-property-by-copy 'footnote-text))
        (stencil . ,ly:balloon-interface::print)
        (text . ,(grob::calc-property-by-copy 'text))
        (X-extent . #f)
        (Y-extent . #f)
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (meta . ((class . Item)
                 (interfaces . (balloon-interface
                                footnote-interface
                                font-interface
                                text-interface))))))

    (FootnoteSpanner
     . (
        (annotation-balloon . #f)
        (annotation-line . #t)
        (automatically-numbered . ,(grob::calc-property-by-copy 'automatically-numbered))
        (footnote . #t)
        (footnote-text . ,(grob::calc-property-by-copy 'footnote-text))
        (spanner-placement . ,LEFT)
        (stencil . ,ly:balloon-interface::print-spanner)
        (text . ,(grob::calc-property-by-copy 'text))
        (X-extent . #f)
        (Y-extent . #f)
        (X-offset . ,(grob::calc-property-by-copy 'X-offset))
        (Y-offset . ,(grob::calc-property-by-copy 'Y-offset))
        (meta . ((class . Spanner)
                 (interfaces . (balloon-interface
                                footnote-interface
                                footnote-spanner-interface
                                font-interface
                                text-interface))))))

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
                                rhythmic-grob-interface))))))


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
        (simple-Y . #t)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-extent . #f)
        (Y-extent . #f)
        (zigzag-width . 0.75)
        (meta . ((class . Spanner)
                 (interfaces . (glissando-interface
                                line-interface
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
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
        (stencil . ,ly:grid-line-interface::print)
        (X-extent . ,ly:grid-line-interface::width)
        (X-offset . ,ly:self-alignment-interface::aligned-on-x-parent)
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
        (broken-bound-padding . ,ly:hairpin::broken-bound-padding)
        (circled-tip . #f)
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
                                self-alignment-interface
                                spanner-interface))))))

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
                                side-position-interface
                                spanner-interface))))))

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
                 (interfaces . (font-interface
                                horizontal-bracket-text-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))))))

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
                 (interfaces . (font-interface
                                self-alignment-interface
                                side-position-interface
                                system-start-text-interface
                                text-interface))))))

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
                 (interfaces . (font-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))))))


    (KeyCancellation
     . (
        (break-align-symbol . key-cancellation)
        (break-visibility . ,begin-of-line-invisible)
        (glyph-name-alist . ,cancellation-glyph-name-alist)
        (non-musical . #t)
        (flat-positions . (2 3 4 2 1 2 1))
        (sharp-positions . (4 5 4 2 3 2 3))
        (space-alist . (
                        (time-signature . (extra-space . 1.25))
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
                 (interfaces . (break-aligned-interface
                                font-interface
                                key-cancellation-interface
                                key-signature-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))))))

    (KeySignature
     . (
        (avoid-slur . inside)
        (break-align-anchor . ,ly:break-aligned-interface::calc-extent-aligned-anchor)
        (break-align-anchor-alignment . ,RIGHT)
        (break-align-symbol . key-signature)
        (break-visibility . ,begin-of-line-visible)
        (glyph-name-alist . ,standard-alteration-glyph-name-alist)
        (non-musical . #t)
        (flat-positions . (2 3 4 2 1 2 1))
        (sharp-positions . (4 5 4 2 3 2 3))
        (space-alist . (
                        (ambitus . (extra-space . 1.15))
                        (time-signature . (extra-space . 1.15))
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
                 (interfaces . (break-aligned-interface
                                font-interface
                                key-signature-interface
                                pure-from-neighbor-interface
                                staff-symbol-referencer-interface))))))

    (KievanLigature
     . (
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:kievan-ligature::print)
        (padding . 0.5)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                kievan-ligature-interface))))))

    (LaissezVibrerTie
     . (
        (control-points . ,ly:semi-tie::calc-control-points)
        (cross-staff . ,semi-tie::calc-cross-staff)
        (details . ((ratio . 0.333)
                    (height-limit . 1.0)))
        (direction . ,ly:tie::calc-direction)
        (head-direction . ,LEFT)
        (stencil  . ,laissez-vibrer::print)
        (thickness . 1.0)
        (extra-spacing-height . (-0.5 . 0.5))
        (vertical-skylines . ,grob::always-vertical-skylines-from-stencil)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (semi-tie-interface
                                tie-interface))))))

    (LaissezVibrerTieColumn
     . (
        (head-direction . ,ly:semi-tie-column::calc-head-direction)
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
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-extent . #f)
        (Y-extent . #f)
        (meta . ((class . Spanner)
                 (interfaces . (ledger-line-spanner-interface))))))

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
                        (staff-bar . (extra-space . 0.0))
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
                 (interfaces . (break-aligned-interface))))))

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
                                text-interface))))))

    (MeasureCounter
     . (
        (count-from . 1)
        (direction . ,UP)
        (font-encoding . fetaText)
        (font-size . -2)
        (outside-staff-horizontal-padding . 0.5)
        (outside-staff-priority . 750)
        (self-alignment-X . ,CENTER)
        (side-axis . ,Y)
        (spacing-pair . (break-alignment . break-alignment))
        (staff-padding . 0.5)
        (stencil . ,measure-counter-stencil)
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                measure-counter-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))))))

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
                 (interfaces . (font-interface
                                measure-spanner-interface
                                line-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
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
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (measure-grouping-interface
                                outside-staff-interface
                                side-position-interface))))))

    (MelodyItem
     . (
        (neutral-direction . ,DOWN)
        (meta . ((class . Item)
                 (interfaces . (melody-spanner-interface))))))

    (MensuralLigature
     . (
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:mensural-ligature::print)
        (thickness . 1.3)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                mensural-ligature-interface))))))

    (MetronomeMark
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (break-visibility . ,end-of-line-invisible)
        (direction . ,UP)
        (extra-spacing-width . (+inf.0 . -inf.0))
        (flag-style . default)
        (outside-staff-horizontal-padding . 0.2)
        (outside-staff-priority . 1000)
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
                                text-interface))))))

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
                                staff-symbol-referencer-interface))))))

    (MultiMeasureRestNumber
     . (
        (bound-padding  . 1.0)
        (direction . ,UP)
        (font-encoding . fetaText)
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
                                text-interface))))))

    (MultiMeasureRestText
     . (
        (direction . ,UP)
        (outside-staff-priority . 450)
        (padding . 0.2)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
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
                                text-interface))))))

    (MultiMeasureRestScript
     . (
        (direction . ,UP)
        (outside-staff-padding . 0)
        (outside-staff-priority . 40)
        (parent-alignment-X . ,CENTER)
        (self-alignment-X . ,CENTER)
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
                                side-position-interface))))))

    (NonMusicalPaperColumn
     . (
        (allow-loose-spacing . #t)
        (axes . (,X))
        (before-line-breaking . ,ly:paper-column::before-line-breaking)
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
                                paper-column-interface
                                separation-item-interface
                                spaceable-grob-interface))))))

    (NoteCollision
     . (
        (axes . (,X ,Y))
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
                                note-collision-interface))))))

    (NoteColumn
     . (
        (axes . (,X ,Y))
        (bound-alignment-interfaces . (rhythmic-head-interface stem-interface))
        (horizontal-skylines . ,ly:separation-item::calc-skylines)
        (skyline-vertical-padding . 0.15)
        (X-extent . ,ly:axis-group-interface::width)
        (Y-extent . ,axis-group-interface::height)
        (vertical-skylines . ,ly:axis-group-interface::calc-skylines)
        (meta . ((class . Item)
                 (object-callbacks . ((pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)))
                 (interfaces . (axis-group-interface
                                note-column-interface
                                separation-item-interface))))))

    (NoteHead
     . (
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
                 (interfaces . (font-interface
                                gregorian-ligature-interface
                                ledgered-interface
                                ligature-head-interface
                                mensural-ligature-interface
                                note-head-interface
                                rhythmic-grob-interface
                                rhythmic-head-interface
                                staff-symbol-referencer-interface
                                vaticana-ligature-interface))))))

    (NoteName
     . (
        (stencil . ,ly:text-interface::print)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
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
                                text-interface))))))


    (PaperColumn
     . (
        (allow-loose-spacing . #t)
        (axes . (,X))
        (before-line-breaking . ,ly:paper-column::before-line-breaking)
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
                                paper-column-interface
                                separation-item-interface
                                spaceable-grob-interface))))))

    (ParenthesesItem
     . (
        (font-size . -6)
        (padding . 0.2)
        (stencil . ,parentheses-item::print)
        (stencils . ,parentheses-item::calc-parenthesis-stencils)
        ;; X-extent needs to be non-empty in order to allow proper
        ;; horizontal attachment.  ParenthesesItem does not reserve
        ;; space of its own, however.
        (X-extent . (0 . 0))
        (Y-extent . ,parentheses-item::y-extent)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                parentheses-interface))))))

    (PercentRepeat
     . (
        (dot-negative-kern . 0.75)
        (font-encoding . fetaMusic)
        (slope . 1.0)
        (spacing-pair . (break-alignment . staff-bar))
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
        (font-encoding . fetaText)
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
                                percent-repeat-interface
                                outside-staff-interface
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
        (vertical-skylines . ,(ly:make-unpure-pure-container ly:slur::vertical-skylines ly:grob::pure-simple-vertical-skylines-from-extents))
        (Y-extent . ,slur::height)
        (meta . ((class . Spanner)
                 (interfaces . (outside-staff-interface
                                slur-interface))))))

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
                                piano-pedal-interface))))))


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
                 (interfaces . (break-alignable-interface
                                font-interface
                                mark-interface
                                outside-staff-interface
                                self-alignment-interface
                                side-position-interface
                                text-interface))))))

    (RepeatSlash
     . (
        (slash-negative-kern . 0.85)
        (slope . 1.7)
        (stencil . ,ly:percent-repeat-item-interface::beat-slash)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (thickness . 0.48)
        (meta . ((class . Item)
                 (interfaces . (percent-repeat-interface
                                percent-repeat-item-interface
                                rhythmic-grob-interface))))))

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
                 (interfaces . (semi-tie-interface
                                tie-interface))))))

    (RepeatTieColumn
     . (
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
                                staff-symbol-referencer-interface))))))

    (RestCollision
     . (
        (minimum-distance . 0.75)
        (positioning-done . ,ly:rest-collision::calc-positioning-done)
        (meta . ((class . Item)
                 (interfaces . (rest-collision-interface))))))


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
        (vertical-skylines . ,(ly:make-unpure-pure-container ly:slur::vertical-skylines ly:grob::pure-simple-vertical-skylines-from-extents))
        (Y-extent . ,slur::height)
        (meta . ((class . Spanner)
                 (interfaces . (outside-staff-interface
                                slur-interface))))))

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
        (bar-extent . ,axis-group-interface::height)
        (before-line-breaking . ,ly:span-bar::before-line-breaking)
        (break-align-symbol . staff-bar)
        (cross-staff . #t)
        (glyph-name . ,ly:span-bar::calc-glyph-name)
        (layer . 0)
        (non-musical . #t)
        (stencil . ,ly:span-bar::print)
        (X-extent . ,ly:span-bar::width)
        (Y-extent . (+inf.0 . -inf.0))
        (meta . ((class . Item)
                 (interfaces . (bar-line-interface
                                font-interface
                                span-bar-interface))))))

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
                 (interfaces . (pure-from-neighbor-interface))))))

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
        (break-align-symbols . (staff-bar break-alignment))
        (layer . 0)
        (ledger-line-thickness . (1.0 . 0.1))
        (line-count . 5)
        (stencil . ,ly:staff-symbol::print)
        (Y-extent . ,(ly:make-unpure-pure-container ly:staff-symbol::height))
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
        (Y-extent . ,grob::always-Y-extent-from-stencil)
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
                 (interfaces . (stem-interface))))))

    (StemStub
     . (
        (X-extent . ,stem-stub::width)
        (extra-spacing-height . ,stem-stub::extra-spacing-height)
        (Y-extent . ,(ly:make-unpure-pure-container #f stem-stub::pure-height))
        (meta . ((class . Item)
                 (interfaces . ())))))

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
                                stem-tremolo-interface))))))

    (StringNumber
     . (
        (add-stem-support . ,only-if-beamed)
        (avoid-slur . around)
        (cross-staff . ,script-or-side-position-cross-staff)
        (font-encoding . fetaText)
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
                                text-script-interface))))))

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
                                text-script-interface))))))

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
                                side-position-interface))))))

    (System
     . (
        (adjacent-pure-heights . ,ly:axis-group-interface::adjacent-pure-heights)
        (axes . (,X ,Y))
        (outside-staff-placement-directive . left-to-right-polite)
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
                                system-interface
                                outside-staff-axis-group-interface))))))

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
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                side-position-interface
                                system-start-delimiter-interface))))))

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
                                system-start-delimiter-interface))))))


    (TabNoteHead
     . (
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
        (stem-attachment . (0.0 . 1.35))
        (stencil . ,tab-note-head::print)
        (whiteout . #t)
        (X-offset . ,ly:self-alignment-interface::x-aligned-on-self)
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces  . (font-interface
                                 note-head-interface
                                 rhythmic-grob-interface
                                 rhythmic-head-interface
                                 staff-symbol-referencer-interface
                                 tab-note-head-interface
                                 text-interface))))))

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
                 (interfaces . (font-interface
                                instrument-specific-markup-interface
                                outside-staff-interface
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
                          (left-broken . ((attach-dir . ,RIGHT)))
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
        (Y-offset . ,side-position-interface::y-aligned-side)

        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                line-interface
                                line-spanner-interface
                                outside-staff-interface
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
        (line-thickness . 0.8)
        (neutral-direction . ,UP)
        (springs-and-rods . ,ly:spanner::set-spacing-rods)
        (stencil . ,ly:tie::print)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
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
        (Y-extent . ,accidental-interface::height)
        (meta . ((class . Item)
                 (interfaces . (accidental-interface
                                font-interface
                                inline-accidental-interface
                                side-position-interface
                                trill-pitch-accidental-interface))))))

    (TrillPitchGroup
     . (
        (axes . (,X))
        (direction . ,RIGHT)
        (font-size . -4)
        ;; minimum shift to the right, in case the parent note has no stem
        (minimum-space . 2.5)
        (horizon-padding . 0.1) ; to avoid interleaving with augmentation dots
        (padding . 0.3)
        (side-axis . ,X)
        (stencil . ,parenthesize-elements)
        (stencils . ,parentheses-item::calc-parenthesis-stencils)
        (X-offset . ,ly:side-position-interface::x-aligned-side)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
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
        (Y-offset . ,staff-symbol-referencer::callback)
        (Y-extent . ,grob::always-Y-extent-from-stencil)
        (meta . ((class . Item)
                 (interfaces . (font-interface
                                ledgered-interface
                                pitched-trill-interface
                                rhythmic-head-interface
                                staff-symbol-referencer-interface))))))

    (TrillSpanner
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (bound-details . ((left . ((text . ,(make-musicglyph-markup "scripts.trill"))
                                   (Y . 0)
                                   (stencil-offset . (-0.5 . -1))
                                   (padding . 0.5)
                                   (attach-dir . ,CENTER)
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
        (Y-offset . ,side-position-interface::y-aligned-side)
        (meta . ((class . Spanner)
                 (interfaces . (font-interface
                                line-interface
                                line-spanner-interface
                                outside-staff-interface
                                side-position-interface
                                trill-spanner-interface))))))

    (TupletBracket
     . (
        (avoid-scripts . #t)
        (connect-to-neighbor . ,ly:tuplet-bracket::calc-connect-to-neighbors)
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
        (tuplet-slur . #f)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (X-positions . ,ly:tuplet-bracket::calc-x-positions)

        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                outside-staff-interface
                                tuplet-bracket-interface))))))

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
                                tuplet-number-interface))))))


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
        (Y-extent . ,axis-group-interface::height)
        (meta . ((class . Spanner)
                 (object-callbacks . ((Y-common . ,ly:axis-group-interface::calc-y-common)
                                      (pure-relevant-grobs . ,ly:axis-group-interface::calc-pure-relevant-grobs)
                                      (pure-Y-common . ,ly:axis-group-interface::calc-pure-y-common)))
                 (interfaces . (align-interface
                                axis-group-interface))))))

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
        (stencil . ,ly:axis-group-interface::print)
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
                                outside-staff-axis-group-interface))))))

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
        (baseline-skip . 1.7)
        (direction . ,UP)
        (edge-height . (2.0 . 2.0)) ;; staff-space;
        (font-encoding . fetaText)
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
                                volta-interface))))))

    (VoltaBracketSpanner
     . (
        (after-line-breaking . ,ly:side-position-interface::move-to-extremal-staff)
        (axes . (,Y))
        (direction . ,UP)
        (no-alignment . #t)
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
                                volta-interface))))))

    (VowelTransition
     . (
        (after-line-breaking . ,ly:spanner::kill-zero-spanned-time)
        (arrow-length . 0.5)
        (arrow-width . 0.5)
        (bound-details . ((left . ((Y . 0)
                                   (padding . 0.14)
                                   (attach-dir . ,RIGHT)
                                   ))
                          (right-broken . ((padding . 0)))
                          (left-broken . ((padding . 0)))
                          (right . ((Y . 0)
                                    (padding . 0.14)
                                    (attach-dir . ,LEFT)
                                    (arrow . #t)
                                    ))))
        (left-bound-info . ,ly:line-spanner::calc-left-bound-info)
        (minimum-length . 1.0)
        (right-bound-info . ,ly:line-spanner::calc-right-bound-info)
        (springs-and-rods . ,ly:vowel-transition::set-spacing-rods)
        (stencil . ,ly:line-spanner::print)
        (style . line)
        (vertical-skylines . ,grob::unpure-vertical-skylines-from-stencil)
        (Y-offset . 0.5)
        (meta . ((class . Spanner)
                 (interfaces . (line-interface
                                line-spanner-interface
                                lyric-interface
                                spanner-interface))))))

    ))

(define (completize-grob-entry x)
  "Transplant assoc key into 'name entry of 'meta of X.  Set interfaces for Item, Spanner etc.
"
  ;;  (display (car x))
  ;;  (newline)
  (let* ((name-sym  (car x))
         ;; Make (shallow) copies of the list and its items because we modify
         ;; them below.
         (grob-entry (map list-copy (cdr x)))
         (meta-entry (map list-copy (assoc-get 'meta grob-entry)))
         (class (assoc-get 'class meta-entry))
         (ifaces-entry
          (assoc-get 'interfaces meta-entry)))

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
      (ly:warning (_ "Unknown class ~a") class)))

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
