;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(map
 (lambda (sym)
   (set! safe-objects (cons (cons sym (primitive-eval sym))
                            safe-objects)))
 '(ly:add-interface
   ly:moment-add
   ly:all-grob-interfaces
   ly:all-output-backend-commands
   ly:all-stencil-expressions
   ly:bracket
   ly:context-find
   ly:context-id
   ly:context-name
   ly:context-parent
   ly:context-property
   ly:context-property-where-defined
   ly:context-pushpop-property
   ly:context-set-property!
   ly:context-unset-property
   ly:context?
   ly:dimension?
   ly:dir?
   ly:moment-div
   ly:duration-dot-count
   ly:duration-factor
   ly:duration-log
   ly:duration<?
   ly:duration?
   ly:error
   ly:export
   ly:font-get-glyph
   ly:font-design-size
   ly:font-file-name
   ly:font-magnification
   ly:font-metric?
   ly:font-name
   ly:format-output
   ly:get-all-function-documentation
   ly:get-all-translators
   ly:get-glyph
   ly:get-option
   ly:grob-alist-chain
   ly:grob-default-font
   ly:grob-extent
   ly:grob-original
   ly:grob-layout
   ly:grob-parent
   ly:grob-property
   ly:grob-script-priority-less
   ly:grob-set-property!
   ly:grob-staff-position
   ly:grob-suicide!
   ly:grob-system
   ly:grob-translate-axis!
   ly:grob?
   ly:input-file-line-char-column
   ly:input-location?
   ly:input-message
   ly:intlog2
   ly:item-break-dir
   ly:item?
   ly:iterator?
   ly:make-music
   ly:make-duration
   ly:make-moment
   ly:make-music-function
   ly:make-pitch
   ly:make-stencil
   ly:moment<?
   ly:moment?
   ly:moment-mul
   ly:music-compress
   ly:music-deep-copy
   ly:music-duration-compress
   ly:music-duration-length
   ly:music-length
   ly:music-list?
   ly:music-mutable-properties
   ly:music-property
   ly:make-score
   ly:music-set-property!
   ly:music-transpose
   ly:music?
   ly:lily-parser?
   ly:message
   ly:number->string
   ly:option-usage
   ly:output-def-clone
   ly:output-def-lookup
   ly:output-def-scope
   ly:output-description
   ly:paper-book?
   ly:prob-property
   ly:output-def?
   ly:paper-get-font
   ly:paper-get-number
   ly:paper-system?
   ly:parser-parse-string
   ly:pitch-alteration
   ly:pitch-diff
   ly:pitch-notename
   ly:pitch-octave
   ly:pitch-quartertones
   ly:pitch-semitones
   ly:pitch-steps
   ly:pitch-transpose
   ly:pitch<?
   ly:pitch?
   ly:position-on-line?
   ly:round-filled-box
   ly:run-translator
   ly:set-option
   ly:spanner-broken-into
   ly:spanner-bound
   ly:spanner?
   ly:staff-symbol-line-thickness
   ly:stencil-add
   ly:stencil-aligned-to
   ly:stencil-combine-at-edge
   ly:stencil-expr
   ly:stencil-extent
   ly:stencil-translate
   ly:stencil-translate-axis
   ly:stencil?
   ly:text-dimension
   ly:translator-description
   ly:translator-name
   ly:translator?
   ly:transpose-key-alist
   ly:unit
   ly:usage
   ly:version
   ly:warning

   ;; FIXME: cannot change staff size in --safe-mode
   ;;$defaultpaper
   ;;$defaultmidi
   ;;$defaultlayout

   ly:accidental-interface::print
   ly:arpeggio::print
   ly:arpeggio::brew-chord-bracket
   ly:bar-line::print
   ly:breathing-sign::offset-callback
   ly:clef::print
   ly:cluster::print
   ly:cluster-beacon::height
   ly:custos::print
   ly:dots::print
   ly:hairpin::print
   ly:hara-kiri-group-spanner::force-hara-kiri-callback
   ly:hara-kiri-group-spanner::y-extent
   ly:horizontal-bracket::print
   ly:lyric-hyphen::print
   ly:lyric-hyphen::set-spacing-rods
   ly:key-signature-interface::print
   ly:line-spanner::print
   ly:lyric-extender::print
   ly:measure-grouping::print
   ly:mensural-ligature::brew-ligature-primitive
   ly:mensural-ligature::print
   ly:multi-measure-rest::percent
   ly:multi-measure-rest::print
   ly:multi-measure-rest::set-spacing-rods
   ly:note-head::print
   ly:ottava-bracket::print
   ly:paper-column::print
   ly:percent-repeat-item-interface::beat-slash
   ly:percent-repeat-item-interface::double-percent
   ly:piano-pedal-bracket::print
   ly:rest::print
   ly:script-interface::print
   ly:slur::height
   ly:slur::print
   ly:spacing-spanner::set-springs
   ly:span-bar::print
   ly:spanner::set-spacing-rods
   ly:staff-symbol::print
   ly:staff-symbol-referencer::callback
   ly:stem::height
   ly:stem::offset-callback
   ly:stem::print
   ly:stem-tremolo::height
   ly:stem-tremolo::print
   ly:sustain-pedal::print
   ly:system-start-delimiter::print
   ly:text-interface::print
   ly:tie::print
   ly:time-signature::print
   ly:tuplet-bracket::print
   ly:vaticana-ligature::brew-ligature-primitive
   ly:vaticana-ligature::print
   ly:volta-bracket-interface::print
   ))

(define-public (make-safe-lilypond-module)
  (let* ((m (make-safe-module)))
    (for-each (lambda (p) (module-define! m (car p) (cdr p))) safe-objects)
    m))
