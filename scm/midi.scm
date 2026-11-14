;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2026 Jan Nieuwenhuizen <janneke@gnu.org>
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



;; (name . program+32768*(channel10 ? 1 : 0))
(define-public instrument-names-alist
  `(
    ("acoustic grand" . ,(- 1 1))
    ("bright acoustic" . ,(- 2 1))
    ("electric grand" . ,(- 3 1))
    ("honky-tonk" . ,(- 4 1))
    ("electric piano 1" . ,(- 5 1))
    ("electric piano 2" . ,(- 6 1))
    ("harpsichord" . ,(- 7 1))
    ("clav" . ,(- 8 1))

    ;; (9-16 chrom percussion)
    ("celesta" . ,(- 9 1))
    ("glockenspiel" . ,(- 10 1))
    ("music box" . ,(- 11 1))
    ("vibraphone" . ,(- 12 1))
    ("marimba" . ,(- 13 1))
    ("xylophone" . ,(- 14 1))
    ("tubular bells" . ,(- 15 1))
    ("dulcimer" . ,(- 16 1))

    ;; (17-24 organ)
    ("drawbar organ" . ,(- 17 1))
    ("percussive organ" . ,(- 18 1))
    ("rock organ" . ,(- 19 1))
    ("church organ" . ,(- 20 1))
    ("reed organ" . ,(- 21 1))
    ("accordion" . ,(- 22 1))
    ("harmonica" . ,(- 23 1))
    ("concertina" . ,(- 24 1))

    ;; (25-32 guitar)
    ("acoustic guitar (nylon)" . ,(- 25 1))
    ("acoustic guitar (steel)" . ,(- 26 1))
    ("electric guitar (jazz)" . ,(- 27 1))
    ("electric guitar (clean)" . ,(- 28 1))
    ("electric guitar (muted)" . ,(- 29 1))
    ("overdriven guitar" . ,(- 30 1))
    ("distorted guitar" . ,(- 31 1))
    ("guitar harmonics" . ,(- 32 1))

    ;; (33-40 bass)
    ("acoustic bass" . ,(- 33 1))
    ("electric bass (finger)" . ,(- 34 1))
    ("electric bass (pick)" . ,(- 35 1))
    ("fretless bass" . ,(- 36 1))
    ("slap bass 1" . ,(- 37 1))
    ("slap bass 2" . ,(- 38 1))
    ("synth bass 1" . ,(- 39 1))
    ("synth bass 2" . ,(- 40 1))

    ;; (41-48 strings)
    ("violin" . ,(- 41 1))
    ("viola" . ,(- 42 1))
    ("cello" . ,(- 43 1))
    ("contrabass" . ,(- 44 1))
    ("tremolo strings" . ,(- 45 1))
    ("pizzicato strings" . ,(- 46 1))
    ("orchestral harp" . ,(- 47 1))
    ("timpani" . ,(- 48 1))

    ;; (49-56 ensemble)
    ("string ensemble 1" . ,(- 49 1))
    ("string ensemble 2" . ,(- 50 1))
    ("synthstrings 1" . ,(- 51 1))
    ("synthstrings 2" . ,(- 52 1))
    ("choir aahs" . ,(- 53 1))
    ("voice oohs" . ,(- 54 1))
    ("synth voice" . ,(- 55 1))
    ("orchestra hit" . ,(- 56 1))

    ;; (57-64 brass)
    ("trumpet" . ,(- 57 1))
    ("trombone" . ,(- 58 1))
    ("tuba" . ,(- 59 1))
    ("muted trumpet" . ,(- 60 1))
    ("french horn" . ,(- 61 1))
    ("brass section" . ,(- 62 1))
    ("synthbrass 1" . ,(- 63 1))
    ("synthbrass 2" . ,(- 64 1))

    ;; (65-72 reed)
    ("soprano sax" . ,(- 65 1))
    ("alto sax" . ,(- 66 1))
    ("tenor sax" . ,(- 67 1))
    ("baritone sax" . ,(- 68 1))
    ("oboe" . ,(- 69 1))
    ("english horn" . ,(- 70 1))
    ("bassoon" . ,(- 71 1))
    ("clarinet" . ,(- 72 1))

    ;; (73-80 pipe)
    ("piccolo" . ,(- 73 1))
    ("flute" . ,(- 74 1))
    ("recorder" . ,(- 75 1))
    ("pan flute" . ,(- 76 1))
    ("blown bottle" . ,(- 77 1))
    ("shakuhachi" . ,(- 78 1))
    ("whistle" . ,(- 79 1))
    ("ocarina" . ,(- 80 1))

    ;; (81-88 synth lead)
    ("lead 1 (square)" . ,(- 81 1))
    ("lead 2 (sawtooth)" . ,(- 82 1))
    ("lead 3 (calliope)" . ,(- 83 1))
    ("lead 4 (chiff)" . ,(- 84 1))
    ("lead 5 (charang)" . ,(- 85 1))
    ("lead 6 (voice)" . ,(- 86 1))
    ("lead 7 (fifths)" . ,(- 87 1))
    ("lead 8 (bass+lead)" . ,(- 88 1))

    ;; (89-96 synth pad)
    ("pad 1 (new age)" . ,(- 89 1))
    ("pad 2 (warm)" . ,(- 90 1))
    ("pad 3 (polysynth)" . ,(- 91 1))
    ("pad 4 (choir)" . ,(- 92 1))
    ("pad 5 (bowed)" . ,(- 93 1))
    ("pad 6 (metallic)" . ,(- 94 1))
    ("pad 7 (halo)" . ,(- 95 1))
    ("pad 8 (sweep)" . ,(- 96 1))

    ;; (97-104 synth effects)
    ("fx 1 (rain)" . ,(- 97 1))
    ("fx 2 (soundtrack)" . ,(- 98 1))
    ("fx 3 (crystal)" . ,(- 99 1))
    ("fx 4 (atmosphere)" . ,(- 100 1))
    ("fx 5 (brightness)" . ,(- 101 1))
    ("fx 6 (goblins)" . ,(- 102 1))
    ("fx 7 (echoes)" . ,(- 103 1))
    ("fx 8 (sci-fi)" . ,(- 104 1))

    ;; (105-112 ethnic)
    ("sitar" . ,(- 105 1))
    ("banjo" . ,(- 106 1))
    ("shamisen" . ,(- 107 1))
    ("koto" . ,(- 108 1))
    ("kalimba" . ,(- 109 1))
    ("bagpipe" . ,(- 110 1))
    ("fiddle" . ,(- 111 1))
    ("shanai" . ,(- 112 1))

    ;; (113-120 percussive)
    ("tinkle bell" . ,(- 113 1))
    ("agogo" . ,(- 114 1))
    ("steel drums" . ,(- 115 1))
    ("woodblock" . ,(- 116 1))
    ("taiko drum" . ,(- 117 1))
    ("melodic tom" . ,(- 118 1))
    ("synth drum" . ,(- 119 1))
    ("reverse cymbal" . ,(- 120 1))

    ;; (121-128 sound effects)
    ("guitar fret noise" . ,(- 121 1))
    ("breath noise" . ,(- 122 1))
    ("seashore" . ,(- 123 1))
    ("bird tweet" . ,(- 124 1))
    ("telephone ring" . ,(- 125 1))
    ("helicopter" . ,(- 126 1))
    ("applause" . ,(- 127 1))
    ("gunshot" . ,(- 128 1))

    ;; (channel 10 drum-kits - subtract 32768 to get program no.)
    ("standard kit" .     ,(+ 32768 0))
    ("standard drums" .   ,(+ 32768 0))
    ("drums" .            ,(+ 32768 0))
    ("room kit" .         ,(+ 32768 8))
    ("room drums" .       ,(+ 32768 8))
    ("power kit" .        ,(+ 32768 16))
    ("power drums" .      ,(+ 32768 16))
    ("rock drums" .       ,(+ 32768 16))
    ("electronic kit" .   ,(+ 32768 24))
    ("electronic drums" . ,(+ 32768 24))
    ("tr-808 kit" .       ,(+ 32768 25))
    ("tr-808 drums" .     ,(+ 32768 25))
    ("jazz kit" .         ,(+ 32768 32))
    ("jazz drums" .       ,(+ 32768 32))
    ("brush kit" .        ,(+ 32768 40))
    ("brush drums" .      ,(+ 32768 40))
    ("orchestra kit" .    ,(+ 32768 48))
    ("orchestra drums" .  ,(+ 32768 48))
    ("classical drums" .  ,(+ 32768 48))
    ("sfx kit" .          ,(+ 32768 56))
    ("sfx drums" .        ,(+ 32768 56))
    ("mt-32 kit" .        ,(+ 32768 127))
    ("mt-32 drums" .      ,(+ 32768 127))
    ("cm-64 kit" .        ,(+ 32768 127))
    ("cm-64 drums" .      ,(+ 32768 127))
    ))

(define-public (percussion? instrument)
  "Return @code{#t} if the instrument should use MIDI channel 9."

  (let* ((inst  (symbol->string instrument))
         (entry (assoc-get inst instrument-names-alist)))
    (and entry (>= entry 32768))))

(define-public (midi-program instrument)
  "Return the program of the instrument."

  (let* ((inst  (symbol->string instrument))
         (entry (assoc-get inst instrument-names-alist)))
    (if entry
        (modulo entry 32768)
        #f)))

;; 90 == 90/127 == 0.71 is supposed to be the default value
;; urg: we should set this at start of track
(define-public dynamic-default-volume 0.71)

(define-public (alterations-in-key pitch-list)
  "Count number of sharps minus number of flats."

  (apply + (map (lambda (p) (round (* (cdr p) 2))) pitch-list)) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (performance-name-from-headers headers)
  (markup->string (or (ly:modules-lookup headers 'midititle)
                      (ly:modules-lookup headers 'title)
                      "")))

(define (midi-block-outfile-name midi-def)
  "Return the file name (without extension and number) requested by the
@code{output-filename} and @code{output-suffix} variables of the
@code{\\midi} block @var{midi-def}, or @code{#f} if it sets neither.
A variable that is not set there is taken from the book.  A suffix
of @code{#f} suppresses the book's suffix."
  (and midi-def
       current-outfile-parts
       (let* ((filename (ly:output-def-lookup midi-def 'output-filename))
              (suffix (ly:output-def-lookup midi-def 'output-suffix))
              (filename-set? (string? filename))
              (suffix-set? (or (string? suffix) (eq? suffix #f))))
         (and (or filename-set? suffix-set?)
              (let ((base (if filename-set?
                              filename
                              (car current-outfile-parts)))
                    (suffix (if suffix-set?
                                suffix
                                (cadr current-outfile-parts))))
                (if (string? suffix)
                    (format #f "~a-~a" base suffix)
                    base))))))

(define (disambiguate-midi-name name always-number?)
  "Return @var{name}, with an increasing number appended if @var{name}
has already been used for a MIDI file during this run or if
@var{always-number?} is true.  MIDI file names are counted separately
from those of other output files."
  (let* ((counter-alist (ly:parser-lookup 'midi-counter-alist))
         (count (assoc-get name counter-alist 0))
         (count (if always-number? (max count 1) count)))
    (ly:parser-define! 'midi-counter-alist
                       (assoc-set! counter-alist name (1+ count)))
    (if (> count 0)
        (format #f "~a-~a" name count)
        name)))

;; The numbering of MIDI files is kept in a counter per file name, so
;; the optional argument (the number of performances already written for
;; the book, given when output is split into bookparts) is not needed.
(define-public (write-performances-midis performances basename . rest)
  (let ((midi-ext (ly:get-option 'midi-extension)))
    (for-each
     (lambda (perf)
       (let* ((own-name (midi-block-outfile-name
                         (ly:performance-output-def perf)))
              ;; A book whose output file name carries a number gets
              ;; numbered MIDI files throughout.
              (name (if own-name
                        (disambiguate-midi-name own-name #f)
                        (disambiguate-midi-name
                         basename
                         (and current-outfile-parts
                              (caddr current-outfile-parts))))))
         (ly:performance-write
          perf
          (format #f "~a.~a" name midi-ext)
          (performance-name-from-headers (ly:performance-headers perf)))))
     performances)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating MIDI messages from Scheme performers.
;;
;; These procedures are meant to be called from the callbacks of a
;; translator created by @code{make-performer} (or any @code{is-midi}
;; translator that is not also an @code{is-layout} translator).  They emit
;; their messages at the current time step, in the track and on the channel
;; of the enclosing @code{Staff} or @code{Voice}.  The optional @var{cause}
;; is a stream event used for diagnostics, or @code{#f}.

(define-public midi-controller-numbers
  '((bank-select-msb . 0)
    (modulation . 1)
    (breath . 2)
    (foot . 4)
    (portamento-time . 5)
    (data-entry-msb . 6)
    (volume . 7)
    (balance . 8)
    (pan . 10)
    (expression . 11)
    (bank-select-lsb . 32)
    (data-entry-lsb . 38)
    (sustain . 64)
    (portamento . 65)
    (sostenuto . 66)
    (soft . 67)
    (legato . 68)
    (hold-2 . 69)
    (reverb . 91)
    (tremolo . 92)
    (chorus . 93)
    (detune . 94)
    (phaser . 95)
    (data-increment . 96)
    (data-decrement . 97)
    (nrpn-lsb . 98)
    (nrpn-msb . 99)
    (rpn-lsb . 100)
    (rpn-msb . 101)
    (all-sound-off . 120)
    (reset-all-controllers . 121)
    (local-control . 122)
    (all-notes-off . 123)))

(define (midi-controller-number controller)
  (cond ((integer? controller)
         (if (<= 0 controller 127)
             controller
             (error "MIDI controller number out of range 0--127:" controller)))
        ((assq-ref midi-controller-numbers controller))
        (else
         (error "unknown MIDI controller name:" controller))))

(define (check-midi-14bit-value value what)
  (if (not (and (integer? value) (<= 0 value 16383)))
      (error (format #f "MIDI ~a out of range 0--16383:" what) value)))

(define*-public (midi-announce-controller performer controller value
                                          #:optional cause)
  "Announce a MIDI control change.  @var{controller} is a number in the
range 0--127 or one of the symbols listed in
@code{midi-controller-numbers}, such as @code{pan}, @code{volume},
@code{expression}, @code{sustain}, @code{reverb} or @code{chorus}.
@var{value} is in the range 0--127."
  (ly:performer-announce-midi-channel
   performer cause #xB0
   (list (midi-controller-number controller) value)))

(define*-public (midi-announce-controller-14bit performer controller value
                                                #:optional cause)
  "Announce a 14-bit MIDI control change as two messages: the most
significant byte on @var{controller} (a number in the range 0--31 or the
name of such a controller, e.g. @code{volume}), then the least significant
byte on the controller 32 higher.  @var{value} is in the range 0--16383."
  (let ((msb-controller (midi-controller-number controller)))
    (if (> msb-controller 31)
        (error "a 14-bit MIDI controller must be in the range 0--31:"
               controller))
    (check-midi-14bit-value value "controller value")
    (midi-announce-controller performer msb-controller
                              (quotient value 128) cause)
    (midi-announce-controller performer (+ msb-controller 32)
                              (modulo value 128) cause)))

(define*-public (midi-announce-pitch-bend performer value #:optional cause)
  "Announce a MIDI pitch bend message.  @var{value} is in the range
0--16383, where 8192 means no bend."
  (check-midi-14bit-value value "pitch bend value")
  (ly:performer-announce-midi-channel
   performer cause #xE0
   (list (modulo value 128) (quotient value 128))))

(define*-public (midi-announce-channel-pressure performer pressure
                                                #:optional cause)
  "Announce a MIDI channel pressure (channel aftertouch) message.
@var{pressure} is in the range 0--127."
  (ly:performer-announce-midi-channel performer cause #xD0 (list pressure)))

(define*-public (midi-announce-poly-pressure performer note pressure
                                             #:optional cause)
  "Announce a MIDI polyphonic key pressure (polyphonic aftertouch) message
for MIDI note number @var{note}.  @var{pressure} is in the range 0--127."
  (ly:performer-announce-midi-channel performer cause #xA0
                                      (list note pressure)))

(define*-public (midi-announce-program-change performer program
                                              #:optional cause)
  "Announce a MIDI program change.  @var{program} is in the range 0--127."
  (ly:performer-announce-midi-channel performer cause #xC0 (list program)))

;; Send a registered or non-registered parameter number, its value, and then
;; the null parameter number, so that later data entry messages cannot
;; modify the parameter by accident.
(define (announce-midi-parameter performer msb-controller lsb-controller
                                 parameter value cause)
  (check-midi-14bit-value parameter "parameter number")
  (check-midi-14bit-value value "parameter value")
  (let ((send (lambda (controller data)
                (midi-announce-controller performer controller data cause))))
    (send msb-controller (quotient parameter 128))
    (send lsb-controller (modulo parameter 128))
    (send 'data-entry-msb (quotient value 128))
    (send 'data-entry-lsb (modulo value 128))
    (send 'rpn-msb 127)
    (send 'rpn-lsb 127)))

(define*-public (midi-announce-rpn performer parameter value #:optional cause)
  "Announce the setting of a registered parameter (RPN) as a sequence of
control changes.  @var{parameter} and @var{value} are 14-bit numbers in the
range 0--16383.  The sequence ends with the null parameter number."
  (announce-midi-parameter performer 'rpn-msb 'rpn-lsb parameter value cause))

(define*-public (midi-announce-nrpn performer parameter value #:optional cause)
  "Announce the setting of a non-registered parameter (NRPN) as a sequence
of control changes.  @var{parameter} and @var{value} are 14-bit numbers in
the range 0--16383.  The sequence ends with the null parameter number."
  (announce-midi-parameter performer 'nrpn-msb 'nrpn-lsb parameter value cause))

(define*-public (midi-announce-pitch-bend-range performer semitones
                                                #:optional (cents 0) cause)
  "Announce a change of the pitch bend range of the channel to
@var{semitones} (and optionally @var{cents}), using registered parameter 0."
  (midi-announce-rpn performer 0 (+ (* 128 semitones) cents) cause))

(define*-public (midi-announce-gm-system-on performer #:optional cause)
  "Announce the General MIDI system on message, a system exclusive
message."
  (ly:performer-announce-midi-sysex performer cause
                                    '(#x7e #x7f #x09 #x01)))
