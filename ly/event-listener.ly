%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011 Graham Percival <graham@percival-music.ca>
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



\version "2.15.0"

%%%% Helper functions

#(define (filename-from-staffname engraver)
   "Constructs a filename in the form
@file{@var{original_filename}-@var{staff_instrument_name}.notes} if the
staff has an instrument name.  If the staff has no instrument
name, it uses "unnamed-staff" for that part of the filename."
   (let* ((inst-name (ly:context-property
                      (ly:translator-context engraver)
                      'instrumentName)))
     (string-concatenate (list
                          (substring (object->string (command-line))
                           ;; filename without .ly part
                           (+ (string-rindex (object->string (command-line)) #\sp) 2)
                           (- (string-length (object->string (command-line))) 5))
                          "-"
                          (if (string? inst-name)
                              inst-name
                            "unnamed-staff")
                          ".notes"))))

#(define (format-moment moment)
   (exact->inexact
    (/ (ly:moment-main-numerator moment)
       (ly:moment-main-denominator moment))))

#(define (adjust-for-grace moment)
   "Adjusts any moment with a grace note by subtracting half of
the grace note duration.  For example, an eighth note grace note
which would otherwise occur at score time 0.5 will now occur at
score time 0.375."
   (if
       (eq? 0 (ly:moment-grace-numerator moment))
       moment
       ;; get moment including grace note
       ;; grace notes have a negative numerator, so add
       (ly:moment-add moment
                      ;; make the "grace duration" half as long
                      (ly:moment-mul
                       (ly:make-moment 1 2)
                       (ly:make-moment
                        (ly:moment-grace-numerator moment)
                        (ly:moment-grace-denominator moment))))))

#(define (get-moment moment)
   (format-moment (adjust-for-grace
                   moment)))

#(define (make-output-string-line engraver values)
   "Constructs a tab-separated string beginning with the
score time (derived from the engraver) and then adding all the
values.  The string ends with a newline."
   (let* ((context (ly:translator-context engraver))
          (moment (ly:context-current-moment context)))
    (string-append
     (string-join
      (map
       (lambda (x) (ly:format "~a" x))
        (append
         (list (get-moment moment))
          values))
        "\t")
     "\n")))


#(define (print-line engraver . values)
   "Prints the list of values (plus the score time) to a file, and
optionally outputs to the console as well."
   (let* ((p (open-file (filename-from-staffname engraver) "a")))
     ;; for regtest comparison
    (if (defined? 'EVENT_LISTENER_CONSOLE_OUTPUT)
     (ly:progress
      (make-output-string-line engraver values)))
    (display
     (make-output-string-line engraver values)
     p)
    (close p)))


%%% main functions

#(define (format-rest engraver event)
   (print-line engraver
               "rest"
               (ly:duration->string
                (ly:event-property event 'duration))))

#(define (format-note engraver event)
   (let* ((origin (ly:input-file-line-char-column
                   (ly:event-property event 'origin))))
     (print-line engraver
                 "note"
                 ;; get a MIDI pitch value.
                 (+ 60 (ly:pitch-semitones
                        (ly:event-property event 'pitch)))
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
          (spanner-props (ly:context-property context 'TextSpanner))
          (details (chain-assoc-get 'bound-details spanner-props))
          (left-props (assoc-get 'left details '()))
          (left-text (assoc-get 'text left-props '())))
     (print-line engraver
                 "set_string"
                 (ly:event-property event 'span-direction)
                 left-text)))


%%%% The actual engraver definition: We just install some listeners so we
%%%% are notified about all notes and rests. We don't create any grobs or
%%%% change any settings.

\layout {
  \context {
  \Voice
  \consists #(list
              (cons 'listeners
                    (list
                     (cons 'tempo-change-event format-tempo)
                     (cons 'rest-event format-rest)
                     (cons 'note-event format-note)
                     (cons 'articulation-event format-articulation)
                     (cons 'text-script-event format-text)
                     (cons 'slur-event format-slur)
                     (cons 'breathing-event format-breathe)
                     (cons 'dynamic-event format-dynamic)
                     (cons 'crescendo-event format-cresc)
                     (cons 'decrescendo-event format-decresc)
                     (cons 'text-span-event format-textspan)
                     )))
  }
}
