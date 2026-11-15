;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2025--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define (Toe_heel_engraver context)
  (make-engraver
   (listeners
    ((articulation-event engraver event)
     (let ((articulation-type (ly:event-property event 'articulation-type)))
       (when articulation-type
         (let ((selector (case articulation-type
                           ((rtoe) first)
                           ((ltoe) second)
                           ((rheel) third)
                           ((lheel) fourth)
                           (else #f))))
           (when selector
             (let* ((toe-heel-style (ly:context-property
                                     context 'toeHeelStyle 'default))
                    (toe-heel-entry (assoc-get
                                     toe-heel-style toe-heel-styles))
                    (art-dir (selector toe-heel-entry))
                    (tweaks (ly:event-property event 'tweaks)))
               (ly:event-set-property!
                event 'articulation-type (car art-dir))
               ;; Enforce direction so that `^` and friends are
               ;; ignored (including user tweaks).
               (ly:event-set-property!
                event 'tweaks (append
                               tweaks
                               (list (cons 'direction (cdr art-dir))))))))))))

   (acknowledgers
    ;; NB 'script-interface is currently used in
    ;; AccidentalSuggestion, CaesuraScript, DynamicText,
    ;; MultiMeasureRestScript and Script, thus we use
    ;; 'ledgered-grob-interface (used for Script and Custos).
    ((ledgered-grob-interface engraver grob source-engraver)
     (let ((toe-heel-style
            (ly:context-property context 'toeHeelStyle 'default)))
       (when (grob::has-interface grob 'script-interface)
         (ly:grob-set-property! grob 'toe-heel-style toe-heel-style)))))))

(ly:register-translator
 Toe_heel_engraver 'Toe_heel_engraver
 '((grobs-created . ())
   (events-accepted . (articulation-event))
   (properties-read . (toeHeelStyle))
   (properties-written . ())
   (description . "Read the @code{toeHeelStyle} context property and use it to
style @code{\\rtoe} and its siblings, based on the data in the
@code{toe-heel-styles} alist.")))
