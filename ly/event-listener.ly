%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2022 Graham Percival <graham@percival-music.ca>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
%
%
%
% This file is used for Vivi, the Virtual Violinist:
%   http://percival-music.ca/vivi.html
% but it may be helpful to other researchers, either with the same
% output, or as a basis for other work in extracting music events
% from lilypond.
%
% Output format is tab-separated lines, like this:
%0.00000000	note	57	0.25000000	point-and-click 2 38
%0.00000000	dynamic	f
%0.25000000	note	62	0.25000000	point-and-click 7 38
%0.50000000	note	66	0.12500000	point-and-click 9 38
%0.50000000	script	staccato



\version "2.16.0"

%%%% Helper functions

#(define (filename-from-staffname context)
   "Constructs a filename in the form
@file{@var{original_filename}-@var{staff_instrument_name}.notes} if the
staff has an instrument name.  If the staff has no instrument
name, it uses \"unnamed-staff\" for that part of the filename."
   (let* ((inst-name (ly:context-property context 'instrumentName))
          (original-file-name (ly:parser-output-name))
         )
     (string-concatenate (list
                          ;; filename without .ly part
                          original-file-name
                          "-"
                          (if (string? inst-name)
                              inst-name
                            "unnamed-staff")
                          ".notes"))))

#(define (format-moment moment)
   (exact->inexact
    (/ (ly:moment-main-numerator moment)
       (ly:moment-main-denominator moment))))

#(define (moment-grace->string moment)
   "Prints a moment without grace note(s) as a float such as
0.25000.  Grace notes are written with the grace duration as a
separate \"dashed\" number, i.e. 0.25000-0.12500.  This allows any
program using the output of this function to interpret grace notes
however they want (half duration, quarter duration?  before beat,
after beat?  etc.)."
   (if
       (zero? (ly:moment-grace-numerator moment))
       (ly:format "~a" (format-moment moment))
       ;; grace notes have a negative numerator, so no "-" necessary
       (ly:format
         "~a~a"
         (format-moment moment)
         (format-moment
                       (ly:make-moment
                        (ly:moment-grace-numerator moment)
                        (ly:moment-grace-denominator moment))))))

#(define (make-output-string-line context values)
   "Constructs a tab-separated string beginning with the
score time (derived from the context) and then adding all the
values.  The string ends with a newline."
   (let* ((moment (ly:context-current-moment context)))
    (string-append
     (string-join
       (append
         (list (moment-grace->string moment))
         (map
             (lambda (x) (ly:format "~a" x))
             values))
       "\t")
     "\n")))


#(define (print-line context . values)
   "Prints the list of values (plus the score time) to a file, and
optionally outputs to the console as well.  context may be specified
as an engraver for convenience."
   (if (ly:translator? context)
       (set! context (ly:translator-context context)))
   (let* ((p (open-file (filename-from-staffname context) "a")))
     ;; for regtest comparison
    (if (defined? 'EVENT_LISTENER_CONSOLE_OUTPUT)
     (ly:progress
      (make-output-string-line context values)))
    (display
     (make-output-string-line context values)
     p)
    (close p)))


%%% main functions

#(define (format-rest engraver event)
   (print-line engraver
               "rest"
               (ly:duration->string
                (ly:event-property event 'duration))
               (format-moment (ly:duration-length
                               (ly:event-property event 'duration)))))

#(define (format-note engraver event)
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property event 'origin)))
          (drum-type (ly:event-property event 'drum-type))
          (pitch (ly:event-property event 'pitch)))
     (print-line engraver
                 (if (ly:pitch? pitch)
                     "note"
                     "type")
                 (if (ly:pitch? pitch)
                     ;; get a MIDI pitch value.
                     (+ 60 (ly:pitch-semitones
                            (ly:event-property event 'pitch)))
                     drum-type)
                 (ly:duration->string
                  (ly:event-property event 'duration))
                 (format-moment (ly:duration-length
                                 (ly:event-property event 'duration)))
                 ;; point and click info
                 (ly:format "point-and-click ~a ~a"
                            (caddr origin)
                            (cadr origin)))))

#(define (format-tempo engraver event)
   (print-line engraver
               "tempo"
               ; get length of quarter notes, in seconds
               (/ (ly:event-property event 'metronome-count)
                   (format-moment (ly:duration-length (ly:event-property
                                                       event
                                                       'tempo-unit))))))


#(define (format-breathe engraver event)
   (print-line engraver
               "breathe"))

#(define (format-glissando engraver event)
   (print-line engraver
               "gliss"))

#(define (format-tie engraver event)
   (print-line engraver
               "tie"))

#(define (format-articulation engraver event)
   (print-line engraver
               "script"
               (ly:event-property event 'articulation-type)))

#(define (format-text engraver event)
   (print-line engraver
               "text"
               (ly:event-property event 'text)))

#(define (format-slur engraver event)
   (print-line engraver
               "slur"
               (ly:event-property event 'span-direction)))

#(define (format-dynamic engraver event)
   (print-line engraver
               "dynamic"
               (ly:event-property event 'text)))

#(define (format-cresc engraver event)
   (print-line engraver
               "cresc"
               (ly:event-property event 'span-direction)))

#(define (format-decresc engraver event)
   (print-line engraver
               "decresc"
               (ly:event-property event 'span-direction)))

#(define (format-textspan engraver event)
   (let* ((context (ly:translator-context engraver))
          (moment (ly:context-current-moment context))
          (spanner-props (ly:context-grob-definition context 'TextSpanner))
          (details (assoc-get 'bound-details spanner-props))
          (left-props (assoc-get 'left details '()))
          (left-text (assoc-get 'text left-props '())))
     (print-line engraver
                 "set_string"
                 (ly:event-property event 'span-direction)
                 left-text)))


%%%% The actual engraver definition: We just install some listeners so we
%%%% are notified about all notes and rests. We don't create any grobs or
%%%% change any settings.

#(define event-listener-engraver
  (make-engraver
    (listeners
     (tempo-change-event . format-tempo)
     (rest-event . format-rest)
     (note-event . format-note) ;; works for notes and drum notes
     (articulation-event . format-articulation)
     (text-script-event . format-text)
     (slur-event . format-slur)
     (breathing-event . format-breathe)
     (dynamic-event . format-dynamic)
     (crescendo-event . format-cresc)
     (decrescendo-event . format-decresc)
     (text-span-event . format-textspan)
     (glissando-event . format-glissando)
     (tie-event . format-tie))))

\layout {
  \context {
    \Voice
    \consists #event-listener-engraver
  }
  \context {
    \DrumVoice
    \consists #event-listener-engraver
  }
}
