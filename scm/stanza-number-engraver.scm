;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2017--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define (Stanza_number_engraver context)
  (let ((stanza-grob #f)
        (stanza-event #f)
        (create-new-stanza #f))
   (define (create-stanza-grob engraver)
     (set! stanza-grob (ly:engraver-make-grob engraver 'StanzaNumber stanza-event)))
   (make-engraver
    (listeners
     ((stanza-event engraver event)
      (set! create-new-stanza (ly:event-property event 'text #f))
      (set! stanza-event (and create-new-stanza event))))
    ((process-music engraver)
     (cond
      (create-new-stanza
       (create-stanza-grob engraver)
       (ly:grob-set-property! stanza-grob 'is-reminder #f))
      ((and stanza-event (ly:context-property context 'stanzaReminders #f))
       ;; We have ruled out the straightforward cases of not having text.  But
       ;; there may still be grob callbacks involved, and to be able to
       ;; test-drive them, we have to create a grob even if we may end up
       ;; destroying it afterwards.
       (create-stanza-grob engraver)
       (ly:grob-set-property! stanza-grob 'is-reminder #t)
       (let*
        ((stanza-reminder
          (ly:context-property context 'stanzaReminderText #f))
         (current-stanza-text (ly:grob-property stanza-grob 'text))
         (stanza-reminder-text
          (cond
           ((markup? stanza-reminder)
            stanza-reminder)
           ((and (procedure? stanza-reminder)
                 (markup? current-stanza-text))
            (stanza-reminder current-stanza-text))
           (else current-stanza-text))))
        (if (markup? stanza-reminder-text)
            (begin
              (ly:grob-set-property! stanza-grob 'text stanza-reminder-text)
              ;; set grob property 'once-per-line' to #t,
              ;; but do so in a way that respects explicit \override's.
              ;;
              ;; FIXME: why should we respect overrides for a property listed
              ;; as internal property?  Users aren't supposed to override them.
              (ly:grob-set-property!
               stanza-grob 'once-per-line
               (ly:grob-property stanza-grob 'once-per-line #t))
              ;; Same procedure for 'include-grace-notes' except that here the
              ;; default when not overriden comes from the context property
              ;; 'includeGraceNotes'
              (ly:grob-set-property!
               stanza-grob 'include-grace-notes
               (ly:grob-property stanza-grob 'include-grace-notes
                                 (ly:context-property context
                                                      'includeGraceNotes))))
            (ly:grob-suicide stanza-grob))))))
    (acknowledgers
     ((lyric-syllable-interface engraver lyric-grob source-engraver)
      ;; This is done more thoroughly by the Stanza_number_align_engraver:
      ;; It makes sure that lyric syllables are neatly aligned across
      ;; *all* Lyrics contexts (i.e., that all lyric syllables appear
      ;; in the side-support-elements array of all stanza numbers occurring
      ;; at the same timestep).
      ;; But if this is not desired, removing the Stanza_number_align_engraver
      ;; from Score should be possible without messing up the alignment of
      ;; stanza numbers in a specific Lyrics context. Hence, we make sure that
      ;; the Stanza numbers at least become aware of their own context's lyric
      ;; syllables.
      (when (and stanza-grob (grob::is-live? stanza-grob))
        (ly:pointer-group-interface::add-grob stanza-grob
                                              'side-support-elements
                                              lyric-grob))))
    ((stop-translation-timestep engraver)
     (set! create-new-stanza #f)
     (set! stanza-grob #f)))))

(ly:register-translator
 Stanza_number_engraver 'Stanza_number_engraver
 '((grobs-created . (StanzaNumber))
   (events-accepted . ())
   (properties-read . (stanzaReminders stanzaReminderText))
   (properties-written . ())
   (description . "Engrave stanza numbers.")))
