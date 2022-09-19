;;; define-music-display-methods.scm -- data for displaying music
;;; expressions using LilyPond notation.
;;;
;;; Copyright (C) 2005--2022 Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display method implementation
;;;

(define-module (lily display-lily))
(use-modules ((ice-9 list)
              #:select (rassoc)))


;;;
;;; Scheme forms
;;;
(define (number-list->lily-string scm-arg)
  (string-join (map number->string scm-arg) ","))

(define (scheme-expr->lily-string scm-arg)
  (cond ((or (number? scm-arg)
             (string? scm-arg)
             (boolean? scm-arg))
         (format #f "~s" scm-arg))
        ((or (symbol? scm-arg)
             (list? scm-arg))
         (format #f "'~s" scm-arg))
        ((procedure? scm-arg)
         (format #f "~a"
                 (or (procedure-name scm-arg)
                     (with-output-to-string
                       (lambda ()
                         (pretty-print (procedure-source scm-arg)))))))
        (else
         (format #f "~a"
                 (with-output-to-string
                   (lambda ()
                     (display-scheme-music scm-arg)))))))

;;;
;;; Markups
;;;

(define-public (markup->lily-string markup-expr)
  "Return a string describing, in LilyPond syntax, the given markup
expression."
  (define (proc->command proc)
    (let ((cmd-markup (symbol->string (procedure-name proc))))
      (substring cmd-markup 0 (- (string-length cmd-markup)
                                 (string-length "-markup")))))
  (define (arg->string arg)
    (cond ((string? arg)
           (format #f "~s" arg))
          ((markup? arg) ;; a markup
           (markup->lily-string-aux arg))
          ((and (pair? arg) (every markup? arg)) ;; a markup list
           (format #f "{~{ ~a~}}" (map-in-order markup->lily-string-aux arg)))
          (else          ;; a scheme argument
           (format #f "#~a" (scheme-expr->lily-string arg)))))
  (define (markup->lily-string-aux expr)
    (if (string? expr)
        (format #f "~s" expr)
        (let ((cmd (car expr))
              (args (cdr expr)))
          (if (eqv? cmd simple-markup) ;; a simple markup
              (format #f "~s" (car args))
              (format #f "\\~a~{ ~a~}"
                      (proc->command cmd)
                      (map-in-order arg->string args))))))
  (cond ((string? markup-expr)
         (format #f "~s" markup-expr))
        ((eqv? (car markup-expr) simple-markup)
         (format #f "~s" (second markup-expr)))
        (else
         (format #f "\\markup ~a"
                 (markup->lily-string-aux markup-expr)))))

;;;
;;; pitch names
;;;

(define-public (note-name->lily-string ly-pitch)
  ;; here we define a custom pitch= function, since we do not want to
  ;; test whether octaves are also equal. (otherwise, we would be using equal?)
  (define (pitch= pitch1 pitch2)
    (and (= (ly:pitch-notename pitch1) (ly:pitch-notename pitch2))
         (= (ly:pitch-alteration pitch1) (ly:pitch-alteration pitch2))))
  (let* ((result (rassoc ly-pitch pitchnames pitch=)))
    (and result (car result))))

(define-public (octave->lily-string pitch)
  (let ((octave (ly:pitch-octave pitch)))
    (cond ((>= octave 0)
           (make-string (1+ octave) #\'))
          ((< octave -1)
           (make-string (1- (* -1 octave)) #\,))
          (else ""))))

;;;
;;; durations
;;;
(define*-public (duration->lily-string ly-duration #:key
                                       (force-duration #f)
                                       (time-scale (*time-scale*)))
  (let ((log2    (ly:duration-log ly-duration))
        (dots    (ly:duration-dot-count ly-duration))
        (scale (ly:duration-scale ly-duration)))
    (if (or force-duration (not (*omit-duration*)))
        (string-append (case log2
                         ((-1) "\\breve")
                         ((-2) "\\longa")
                         ((-3) "\\maxima")
                         (else (number->string (expt 2 log2))))
                       (make-string dots #\.)
                       (let ((end-scale (/ scale time-scale)))
                         (if (= end-scale 1) ""
                             (format #f "*~a" end-scale))))
        "")))

;;;
;;; post events
;;;

(define post-event? (music-type-predicate 'post-event))

(define* (event-direction->lily-string event #:optional (required #t))
  (let ((direction (ly:music-property event 'direction)))
    (cond ((or (not direction) (null? direction) (= CENTER direction))
           (if required "-" ""))
          ((= UP direction) "^")
          ((= DOWN direction) "_")
          (else ""))))

(define-syntax-rule (define-post-event-display-method type (param) direction-required str)
  (define-display-method type (param)
    (format #f "~a~a"
            (event-direction->lily-string param direction-required)
            str)))

(define-syntax-rule (define-span-event-display-method type (param) direction-required str-start str-stop)
  (define-display-method type (param)
    (format #f "~a~a"
            (event-direction->lily-string param direction-required)
            (if (= START (ly:music-property param 'span-direction))
                str-start
                str-stop))))

(define-display-method HyphenEvent (event)
  " --")
(define-display-method ExtenderEvent (event)
  " __")
(define-display-method VowelTransitionEvent (event)
  " \\vowelTransition")
(define-display-method TieEvent (event)
  " ~")
(define-display-method DurationLineEvent (event)
  "\\-")
(define-display-method BendSpanEvent (event)
  "\\^")
(define-display-method BeamForbidEvent (event)
  "\\noBeam")
(define-display-method StringNumberEvent (event)
  (format #f "\\~a" (ly:music-property event 'string-number)))
(define-display-method BreakDynamicSpanEvent (event)
  "\\breakDynamicSpan")
(define-post-event-display-method LaissezVibrerEvent (event) #f
  "\\laissezVibrer")
(define-post-event-display-method RepeatTieEvent (event) #f
  "\\repeatTie")

(define-display-method TremoloEvent (event)
  (let ((tremolo-type (ly:music-property event 'tremolo-type 8)))
    (format #f ":~a" tremolo-type)))

(define-display-method ArticulationEvent (event) #t
  (let* ((articulation  (ly:music-property event 'articulation-type))
         (shorthand
          (case articulation
            ((marcato) "^")
            ((stopped) "+")
            ((tenuto)    "-")
            ((staccatissimo) "!")
            ((accent) ">")
            ((staccato) ".")
            ((portato) "_")
            (else #f))))
    (format #f "~a~:[\\~;~]~a"
            (event-direction->lily-string event shorthand)
            shorthand
            (or shorthand articulation))))

(define-display-method MultiMeasureArticulationEvent (event)
  (music->lily-string (make-music 'ArticulationEvent event)))

(define-post-event-display-method FingeringEvent (event) #t
  (ly:music-property event 'digit))

(define-post-event-display-method TextScriptEvent (event) #t
  (markup->lily-string (ly:music-property event 'text)))

(define-display-method MultiMeasureTextEvent (event)
  (music->lily-string (make-music 'TextScriptEvent event)))

(define-post-event-display-method BendAfterEvent (event) #f
  (format #f "\\bendAfter #~a " (ly:music-property event 'delta-step)))

(define-post-event-display-method FingerGlideEvent (event) #f "\\glide")
(define-post-event-display-method HarmonicEvent (event) #f "\\harmonic")
(define-post-event-display-method GlissandoEvent (event) #f "\\glissando")
(define-post-event-display-method ArpeggioEvent (event) #f "\\arpeggio")
(define-post-event-display-method AbsoluteDynamicEvent (event) #f
  (format #f "\\~a" (ly:music-property event 'text)))

(define-post-event-display-method StrokeFingerEvent (event) #f
  (format #f "\\rightHandFinger #~a " (ly:music-property event 'digit)))

(define-span-event-display-method BeamEvent (event) #f "[" "]")
(define-span-event-display-method SlurEvent (event) #f "(" ")")
(define-span-event-display-method CrescendoEvent (event) #f "\\<" "\\!")
(define-span-event-display-method DecrescendoEvent (event) #f "\\>" "\\!")
(define-span-event-display-method EpisemaEvent (event) #f "\\episemInitium" "\\episemFinis")
(define-span-event-display-method PhrasingSlurEvent (event) #f "\\(" "\\)")
(define-span-event-display-method SustainEvent (event) #f "\\sustainOn" "\\sustainOff")
(define-span-event-display-method SostenutoEvent (event) #f "\\sostenutoOn" "\\sostenutoOff")
(define-span-event-display-method TextSpanEvent (event) #f "\\startTextSpan" "\\stopTextSpan")
(define-span-event-display-method TrillSpanEvent (event) #f "\\startTrillSpan" "\\stopTrillSpan")
(define-span-event-display-method StaffSpanEvent (event) #f "\\startStaff" "\\stopStaff")
(define-span-event-display-method NoteGroupingEvent (event) #f "\\startGroup" "\\stopGroup")
(define-span-event-display-method UnaCordaEvent (event) #f "\\unaCorda" "\\treCorde")

;;;
;;; Void Music
;;;

(define-display-method Music (expr)
  (format #f "##{#}"))

;;;
;;; Graces
;;;

(define-display-method GraceMusic (expr)
  (format #f "\\grace ~a"
          (music->lily-string (ly:music-property expr 'element))))

;; \acciaccatura \appoggiatura \grace
;; TODO: it would be better to compare ?start and ?stop
;; with startAppoggiaturaMusic and stopAppoggiaturaMusic,
;; using a custom music equality predicate.
(define-extra-display-method GraceMusic (expr)
  "Display method for appoggiatura."
  (with-music-match (expr (music
                           'GraceMusic
                           element (music
                                    'SequentialMusic
                                    elements (?start
                                              ?music
                                              ?stop))))
                    ;; we check whether ?start and ?stop look like
                    ;; startAppoggiaturaMusic stopAppoggiaturaMusic
                    (and (with-music-match (?start (music
                                                    'SequentialMusic
                                                    elements ((music
                                                               'EventChord
                                                               elements
                                                               ((music
                                                                 'SlurEvent
                                                                 span-direction START))))))
                                           #t)
                         (with-music-match (?stop (music
                                                   'SequentialMusic
                                                   elements ((music
                                                              'EventChord
                                                              elements
                                                              ((music
                                                                'SlurEvent
                                                                span-direction STOP))))))
                                           (format #f "\\appoggiatura ~a" (music->lily-string ?music))))))


(define-extra-display-method GraceMusic (expr)
  "Display method for acciaccatura."
  (with-music-match (expr (music
                           'GraceMusic
                           element (music
                                    'SequentialMusic
                                    elements (?start
                                              ?music
                                              ?stop))))
                    ;; we check whether ?start and ?stop look like
                    ;; startAcciaccaturaMusic stopAcciaccaturaMusic
                    (and (with-music-match (?start (music
                                                    'SequentialMusic
                                                    elements ((music
                                                               'EventChord
                                                               elements
                                                               ((music
                                                                 'SlurEvent
                                                                 span-direction START)))
                                                              (music
                                                               'ContextSpeccedMusic
                                                               element (music
                                                                        'OverrideProperty
                                                                        grob-property-path '(stroke-style)
                                                                        grob-value "grace"
                                                                        symbol 'Flag)))))
                                           #t)
                         (with-music-match (?stop (music
                                                   'SequentialMusic
                                                   elements ((music
                                                              'ContextSpeccedMusic
                                                              element (music
                                                                       'RevertProperty
                                                                       grob-property-path '(stroke-style)
                                                                       symbol 'Flag))

                                                             (music
                                                              'EventChord
                                                              elements
                                                              ((music
                                                                'SlurEvent
                                                                span-direction STOP))))))
                                           (format #f "\\acciaccatura ~a" (music->lily-string ?music))))))

(define-extra-display-method GraceMusic (expr)
  "Display method for grace."
  (with-music-match (expr (music
                           'GraceMusic
                           element (music
                                    'SequentialMusic
                                    elements (?start
                                              ?music
                                              ?stop))))
                    ;; we check whether ?start and ?stop look like
                    ;; startGraceMusic stopGraceMusic
                    (and (null? (ly:music-property ?start 'elements))
                         (null? (ly:music-property ?stop 'elements))
                         (format #f "\\grace ~a" (music->lily-string ?music)))))

;;;
;;; Music sequences
;;;

(define-display-method SequentialMusic (seq)
  (let ((force-line-break (and (*force-line-break*)
                               ;; hm
                               (> (length (ly:music-property seq 'elements))
                                  (*max-element-number-before-break*))))
        (elements (ly:music-property seq 'elements))
        (chord? (make-music-type-predicate 'EventChord))
        (note-or-chord? (make-music-type-predicate 'EventChord 'NoteEvent
                                                   'LyricEvent 'RestEvent
                                                   'ClusterNoteEvent))
        (cluster? (make-music-type-predicate 'ClusterNoteEvent))
        (note? (make-music-type-predicate 'NoteEvent)))
    (format #f "~a~a{~v%~v_~{~a~^ ~}~v%~v_}"
            (if (any (lambda (e)
                       (or (cluster? e)
                           (and (chord? e)
                                (any cluster? (ly:music-property e 'elements)))))
                     elements)
                "\\makeClusters "
                "")
            (if (*explicit-mode*)
                ;; if the sequence contains EventChord which contains figures ==> figuremode
                ;; if the sequence contains EventChord which contains lyrics ==> lyricmode
                ;; if the sequence contains EventChord which contains drum notes ==> drummode
                (cond ((any (lambda (chord)
                              (any (make-music-type-predicate 'BassFigureEvent)
                                   (ly:music-property chord 'elements)))
                            (filter chord? elements))
                       "\\figuremode ")
                      ((any (lambda (chord)
                              (any (make-music-type-predicate 'LyricEvent)
                                   (cons chord
                                         (ly:music-property chord 'elements))))
                            (filter note-or-chord? elements))
                       "\\lyricmode ")
                      ((any (lambda (chord)
                              (any (lambda (event)
                                     (and (note? event)
                                          (not (null? (ly:music-property event 'drum-type)))))
                                   (cons chord
                                         (ly:music-property chord 'elements))))
                            (filter note-or-chord? elements))
                       "\\drummode ")
                      (else ;; TODO: other modes?
                       ""))
                "")
            (if force-line-break 1 0)
            (if force-line-break (+ 2 (*indent*)) 1)
            (parameterize ((*indent* (+ 2 (*indent*))))
              (map-in-order (lambda (music)
                              (music->lily-string music))
                            elements))
            (if force-line-break 1 0)
            (if force-line-break (*indent*) 1))))

(define-display-method SimultaneousMusic (sim)
  (parameterize ((*indent* (+ 3 (*indent*))))
    (format #f "<< ~{~a ~}>>"
            (map-in-order (lambda (music)
                            (music->lily-string music))
                          (ly:music-property sim 'elements)))))

;;;
;;; Chords
;;;

(define-display-method EventChord (chord)
  ;; event_chord : command_element
  ;;               | note_chord_element

  ;; TODO : tagged post_events
  ;; post_events : ( post_event | tagged_post_event )*
  ;; tagged_post_event: '-' \tag embedded_scm post_event

  (let* ((elements (append (ly:music-property chord 'elements)
                           (ly:music-property chord 'articulations)))
         (chord-repeat (ly:music-property chord 'duration)))
    (call-with-values
        (lambda ()
          (partition (music-type-predicate 'rhythmic-event)
                     elements))
      (lambda (chord-elements other-elements)
        (cond ((pair? chord-elements)
               ;; note_chord_element :
               ;; '<' (notepitch | drumpitch)* '>" duration post_events
               (let ((duration (duration->lily-string (ly:music-property
                                                       (car chord-elements)
                                                       'duration))))
                 ;; Format duration first so that it does not appear on
                 ;; chord elements
                 (format #f "< ~{~a ~}>~a~:{~:[-~;~]~a~^ ~}"
                         (parameterize ((*omit-duration* #t))
                           (map-in-order
                            (lambda (music)
                              (music->lily-string music))
                            chord-elements))
                         duration
                         (map-in-order (lambda (music)
                                         (list
                                          (post-event? music)
                                          (music->lily-string music)))
                                       other-elements))))
              ((ly:duration? chord-repeat)
               (let ((duration (duration->lily-string chord-repeat)))
                 (format #f "q~a~:{~:[-~;~]~a~^ ~}"
                         duration
                         (map-in-order (lambda (music)
                                         (list
                                          (post-event? music)
                                          (music->lily-string music)))
                                       other-elements))))

              ((and (= 1 (length other-elements))
                    (not (post-event? (car other-elements))))
               (format #f (music->lily-string (car other-elements))))
              (else
               (format #f "< >~:{~:[-~;~]~a~^ ~}"
                       (map-in-order (lambda (music)
                                       (list
                                        (post-event? music)
                                        (music->lily-string music)))
                                     other-elements))))))))

(define-display-method MultiMeasureRestMusic (mmrest)
  (format #f "R~a~{~a~^ ~}"
          (duration->lily-string (ly:music-property mmrest 'duration))
          (map-in-order (lambda (music)
                          (music->lily-string music))
                        (ly:music-property mmrest 'articulations))))

(define-display-method SkipMusic (skip)
  (format #f "\\skip ~a" (duration->lily-string (ly:music-property skip 'duration) #:force-duration #t)))

(define-display-method SkippedMusic (skip)
  (format #f "\\skip ~a"
          (music->lily-string (ly:music-property skip 'element))))

(define-display-method OttavaEvent (ottava)
  (format #f "\\ottava #~a" (ly:music-property ottava 'ottava-number)))

;;;
;;; Notes, rests, skips...
;;;

(define (simple-note->lily-string event)
  (format #f "~a~a~a~a~a~a~:{~:[-~;~]~a~}" ; pitchname octave !? octave-check duration optional_rest articulations
          (note-name->lily-string (ly:music-property event 'pitch))
          (octave->lily-string (ly:music-property event 'pitch))
          (let ((forced (ly:music-property event 'force-accidental))
                (cautionary (ly:music-property event 'cautionary)))
            (cond ((and (not (null? forced))
                        forced
                        (not (null? cautionary))
                        cautionary)
                   "?")
                  ((and (not (null? forced)) forced) "!")
                  (else "")))
          (let ((octave-check (ly:music-property event 'absolute-octave)))
            (if (not (null? octave-check))
                (format #f "=~a" (cond ((>= octave-check 0)
                                        (make-string (1+ octave-check) #\'))
                                       ((< octave-check -1)
                                        (make-string (1- (* -1 octave-check)) #\,))
                                       (else "")))
                ""))
          (duration->lily-string (ly:music-property event 'duration))
          (if ((make-music-type-predicate 'RestEvent) event)
              "\\rest" "")
          (map-in-order (lambda (event)
                          (list
                           (post-event? event)
                           (music->lily-string event)))
                        (ly:music-property event 'articulations))))

(define-display-method NoteEvent (note)
  (cond ((not (null? (ly:music-property note 'pitch))) ;; note
         (simple-note->lily-string note))
        ((not (null? (ly:music-property note 'drum-type))) ;; drum
         (format #f "~a~a~{~a~}" (ly:music-property note 'drum-type)
                 (duration->lily-string (ly:music-property note 'duration))
                 (map-in-order (lambda (event)
                                 (music->lily-string event))
                               (ly:music-property note 'articulations))))
        (else
         ;; pure duration
         (format #f "~a~{~a~}"
                 (duration->lily-string (ly:music-property note 'duration)
                                        #:force-duration #t)
                 (map-in-order (lambda (event)
                                 (music->lily-string event))
                               (ly:music-property note 'articulations))))))

(define-display-method ClusterNoteEvent (note)
  (simple-note->lily-string note))

(define-display-method RestEvent (rest)
  (if (not (null? (ly:music-property rest 'pitch)))
      (simple-note->lily-string rest)
      (format #f "r~a~{~a~}"
              (duration->lily-string (ly:music-property rest 'duration))
              (map-in-order (lambda (event)
                              (music->lily-string event))
                            (ly:music-property rest 'articulations)))))

(define-display-method SkipEvent (rest)
  (format #f "s~a~{~a~}"
          (duration->lily-string (ly:music-property rest 'duration))
          (map-in-order (lambda (event)
                          (music->lily-string event))
                        (ly:music-property rest 'articulations))))

(define-display-method RepeatedChord (chord)
  (music->lily-string (ly:music-property chord 'element)))

(define-display-method AdHocMarkEvent (mark)
  (string-append "\\mark "
                 (markup->lily-string (ly:music-property mark 'text))))

(define-display-method CodaMarkEvent (mark)
  (let ((label (ly:music-property mark 'label #f)))
    (string-append "\\codaMark "
                   (if label (value->lily-string label) "\\default"))))

(define-display-method RehearsalMarkEvent (mark)
  (let ((label (ly:music-property mark 'label #f)))
    (string-append "\\mark "
                   (if label (value->lily-string label) "\\default"))))

(define-display-method SegnoMarkEvent (segno)
  (let ((label (ly:music-property segno 'label #f)))
    (string-append "\\segnoMark "
                   (if label (value->lily-string label) "\\default"))))

(define-display-method TextMarkEvent (text-mark)
  (let* ((text (ly:music-property text-mark 'text))
         (is-end-mark
          (eqv? LEFT (ly:music-property text-mark 'horizontal-direction))))
    (string-append (if is-end-mark "\\textEndMark " "\\textMark ")
                   (value->lily-string text))))

(define-display-method AdHocJumpEvent (jump)
  (let ((text (ly:music-property jump 'text)))
    (string-append "\\jump " (value->lily-string text))))

(define-display-method SectionLabelEvent (section-label)
  (let ((text (ly:music-property section-label 'text)))
    (string-append "\\sectionLabel " (value->lily-string text))))

(define-display-method KeyChangeEvent (key)
  (let ((pitch-alist (ly:music-property key 'pitch-alist))
        (tonic (ly:music-property key 'tonic)))
    (if (or (null? pitch-alist)
            (null? tonic))
        "\\key \\default"
        (let ((c-pitch-alist (ly:transpose-key-alist pitch-alist
                                                     (ly:pitch-diff (ly:make-pitch 0 0 0) tonic))))
          (format #f "\\key ~a \\~a~a"
                  (note-name->lily-string (ly:music-property key 'tonic))
                  (any (lambda (mode)
                         (and (equal? (ly:parser-lookup mode) c-pitch-alist)
                              (symbol->string mode)))
                       '(major minor ionian locrian aeolian mixolydian lydian phrygian dorian))
                  (new-line->lily-string))))))

(define-display-method RelativeOctaveCheck (octave)
  (let ((pitch (ly:music-property octave 'pitch)))
    (format #f "\\octaveCheck ~a~a"
            (note-name->lily-string pitch)
            (octave->lily-string pitch))))

(define-display-method VoiceSeparator (sep)
  "\\\\")

(define-display-method LigatureEvent (ligature)
  (if (= START (ly:music-property ligature 'span-direction))
      "\\["
      "\\]"))

(define-display-method BarCheck (check)
  (format #f "|~a" (new-line->lily-string)))

(define-display-method PesOrFlexaEvent (expr)
  "\\~")

(define-display-method BassFigureEvent (figure)
  ;; TODO handle \+, / and friends as well as arbitrary levels of alteration
  (define (bracketify content) (format #f "[~a]" content))
  (let ((alteration (ly:music-property figure 'alteration))
        (alteration-bracket (ly:music-property figure 'alteration-bracket))
        (fig (ly:music-property figure 'figure))
        (bracket-start (ly:music-property figure 'bracket-start))
        (bracket-stop (ly:music-property figure 'bracket-stop)))

    (format #f "~a~a~a~a"
            (if (null? bracket-start) "" "[")
            (cond ((null? fig) "_")
                  ((markup? fig) (second fig)) ;; fig: (<number-markup> "number")
                  (else fig))
            (if (null? alteration)
                ""
                ((if (null? alteration-bracket) identity bracketify)
                 (cond
                  ((= alteration DOUBLE-FLAT) "--")
                  ((= alteration FLAT) "-")
                  ((= alteration NATURAL) "!")
                  ((= alteration SHARP) "+")
                  ((= alteration DOUBLE-SHARP) "++")
                  (else ""))))
            (if (null? bracket-stop) "" "]"))))

(define-display-method LyricEvent (lyric)
  (format #f "~a~{~a~^ ~}"
          (let ((text (ly:music-property lyric 'text)))
            (if (or (string? text)
                    (eqv? (first text) simple-markup))
                ;; a string or a simple markup
                (let ((string (if (string? text)
                                  text
                                  (second text))))
                  (if (string-match "(\"| |[0-9])" string)
                      ;; TODO check exactly in which cases double quotes should be used
                      (format #f "~s" string)
                      string))
                (markup->lily-string text)))
          (map-in-order music->lily-string
                        (ly:music-property lyric 'articulations))))

(define-display-method BreathingEvent (event)
  "\\breathe")

(define-display-method CaesuraEvent (event)
  "\\caesura")

;;;
;;; Staff switches
;;;

(define-display-method ContextChange (m)
  (format #f "\\change ~a = \"~a\""
          (ly:music-property m 'change-to-type)
          (ly:music-property m 'change-to-id)))

;;;

(define-display-method TimeScaledMusic (times)
  (let* ((num (ly:music-property times 'numerator))
         (den (ly:music-property times 'denominator))
         (span (ly:music-property times 'duration #f))
         ;; need to format before changing time scale
         (formatted-span
          (and span (duration->lily-string span #:force-duration #t)))
         (scale (/ num den))
         (time-scale (*time-scale*)))
    (let ((result
           (parameterize ((*force-line-break* #f)
                          (*time-scale* (* time-scale scale)))
             (format #f "\\tuplet ~a/~a ~@[~a ~]~a"
                     den
                     num
                     formatted-span
                     (music->lily-string (ly:music-property times 'element))))))
      result)))

(define-display-method RelativeOctaveMusic (m)
  (format #f "\\absolute ~a"
          (music->lily-string (ly:music-property m 'element))))

(define-display-method TransposedMusic (m)
  (music->lily-string (ly:music-property m 'element)))

;;;
;;; Repeats
;;;

(define (repeat->lily-string expr repeat-type)
  (let* ((main (music->lily-string (ly:music-property expr 'element))))
    (format #f "\\repeat ~a ~a ~a ~a"
            repeat-type
            (ly:music-property expr 'repeat-count)
            main
            (let ((alternatives (ly:music-property expr 'elements)))
              (if (null? alternatives)
                  ""
                  (format #f "\\alternative { ~{~a ~}}"
                          (map-in-order (lambda (music)
                                          (music->lily-string music))
                                        alternatives)))))))

(define-display-method SequentialAlternativeMusic (expr)
  (format #f "\\alternative { ~{~a ~}}"
          (map-in-order (lambda (music)
                          (music->lily-string music))
                        (ly:music-property expr 'elements))))

(define-display-method SegnoRepeatedMusic (expr)
  (repeat->lily-string expr "segno"))

(define-display-method VoltaRepeatedMusic (expr)
  (repeat->lily-string expr "volta"))

(define-display-method UnfoldedRepeatedMusic (expr)
  (repeat->lily-string expr "unfold"))

(define-display-method PercentRepeatedMusic (expr)
  (repeat->lily-string expr "percent"))

(define-display-method TremoloRepeatedMusic (expr)
  (repeat->lily-string expr "tremolo"))

(define-display-method UnfoldedSpeccedMusic (m)
  (format #f "\\unfolded ~a"
          (music->lily-string (ly:music-property m 'element))))

(define-display-method VoltaSpeccedMusic (m)
  (format #f "\\volta ~a ~a"
          (number-list->lily-string (ly:music-property m 'volta-numbers))
          (music->lily-string (ly:music-property m 'element))))

;;;
;;; Contexts
;;;

(define-display-method ContextSpeccedMusic (expr)
  (let ((id    (ly:music-property expr 'context-id))
        (create-new (ly:music-property expr 'create-new))
        (music (ly:music-property expr 'element))
        (operations (ly:music-property expr 'property-operations))
        (ctype (ly:music-property expr 'context-type)))
    (format #f "~a ~a~a~a ~a"
            (if (and (not (null? create-new)) create-new)
                "\\new"
                "\\context")
            ctype
            (if (null? id)
                ""
                (format #f " = ~s" id))
            (if (null? operations)
                ""
                (format #f " \\with {~{~a~}~%~v_}"
                        (parameterize ((*indent* (+ (*indent*) 2)))
                          (map (lambda (op)
                                 (format #f "~%~v_\\~a ~s"
                                         (*indent*)
                                         (first op)
                                         (second op)))
                               operations))
                        (*indent*)))
            (parameterize ((*current-context* ctype))
              (music->lily-string music)))))

;; \after
(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is an \\after expression with a post-event, return
\"\\after ...\". Otherwise, return #f."
  (with-music-match
   (expr (music 'ContextSpeccedMusic
                context-type 'Bottom
                element
                (music 'SimultaneousMusic
                       elements ((music 'SequentialMusic
                                        elements ((music 'SkipMusic
                                                         duration ?delta)
                                                  (music 'EventChord
                                                         elements ?ev)))
                                 ?mus))))
   (format #f "\\after ~a ~a ~a"
           (duration->lily-string ?delta)
           (music->lily-string (car ?ev))
           (music->lily-string ?mus))))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is an \\after expression with a standalone music event, return
\"\\after ...\". Otherwise, return #f."
  (with-music-match
   (expr (music 'ContextSpeccedMusic
                context-type 'Bottom
                element
                (music 'SimultaneousMusic
                       elements ((music 'SequentialMusic
                                        elements ((music 'SkipMusic
                                                         duration ?delta)
                                                  (music 'EventChord)
                                                  ?ev))
                                 ?mus))))
   (format #f "\\after ~a ~a ~a"
           (duration->lily-string ?delta)
           (music->lily-string ?ev)
           (music->lily-string ?mus))))

;; \afterGrace
(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is an \\afterGrace expression, return \"\\afterGrace ...\".
Otherwise, return #f."
  (with-music-match
   (expr (music 'ContextSpeccedMusic
                context-type 'Bottom
                element
                (music 'SimultaneousMusic
                       elements (?main
                                 (music 'SequentialMusic
                                        elements ((music 'SkipMusic
                                                         duration ?delay-dur)
                                                  (music 'GraceMusic
                                                         element ?grace)))))))
   (format #f "\\afterGrace ~a ~a ~a"
           (/ (ly:duration-scale ?delay-dur)
              (ly:moment-main (ly:music-length ?main)))
           (music->lily-string ?main)
           (music->lily-string ?grace))))


;; special cases: \figures \lyrics \drums
(define-extra-display-method ContextSpeccedMusic (expr)
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 create-new #t
                                 property-operations ?op
                                 context-type ?context-type
                                 element ?sequence))
                    (if (null? ?op)
                        (parameterize ((*explicit-mode* #f))
                          (case ?context-type
                            ((FiguredBass)
                             (format #f "\\figures ~a" (music->lily-string ?sequence)))
                            ((Lyrics)
                             (format #f "\\lyrics ~a" (music->lily-string ?sequence)))
                            ((DrumStaff)
                             (format #f "\\drums ~a" (music->lily-string ?sequence)))
                            (else
                             #f)))
                        #f)))

;;; Context properties

(define-extra-display-method ContextSpeccedMusic (expr)
  (let ((element (ly:music-property expr 'element))
        (property-tuning? (make-music-type-predicate 'PropertySet
                                                     'PropertyUnset
                                                     'OverrideProperty
                                                     'RevertProperty))
        (sequence? (make-music-type-predicate 'SequentialMusic)))
    (if (and (ly:music? element)
             (or (property-tuning? element)
                 (and (sequence? element)
                      (every property-tuning? (ly:music-property element 'elements)))))
        (parameterize ((*current-context* (ly:music-property expr 'context-type)))
          (music->lily-string element))
        #f)))

(define-public (value->lily-string arg)
  (cond ((ly:music? arg)
         (music->lily-string arg))
        ((markup? arg)
         (markup->lily-string arg))
        ((ly:duration? arg)
         (format #f "##{ ~a #}" (duration->lily-string arg #:force-duration #t)))
        ((ly:pitch? arg)
         (format #f "~a~a"
                 (note-name->lily-string arg)
                 (octave->lily-string arg)))
        (else
         (format #f "#~a" (scheme-expr->lily-string arg)))))

(define-display-method PropertySet (expr)
  (let ((property (ly:music-property expr 'symbol))
        (value (ly:music-property expr 'value))
        (once (ly:music-property expr 'once)))
    (format #f "~a\\set ~a~a = ~a~a"
            (if (and (not (null? once)))
                "\\once "
                "")
            (if (eq? (*current-context*) 'Bottom)
                ""
                (format #f "~a." (*current-context*)))
            property
            (value->lily-string value)
            (new-line->lily-string))))

(define-display-method PropertyUnset (expr)
  (format #f "~a\\unset ~a~a~a"
          (if (ly:music-property expr 'once #f) "\\once " "")
          (if (eq? (*current-context*) 'Bottom)
              ""
              (format #f "~a." (*current-context*)))
          (ly:music-property expr 'symbol)
          (new-line->lily-string)))

;;; Layout properties

(define-display-method OverrideProperty (expr)
  (let* ((symbol          (ly:music-property expr 'symbol))
         (properties   (ly:music-property expr 'grob-property-path
                                          (list (ly:music-property expr 'grob-property))))
         (value   (ly:music-property expr 'grob-value))
         (once    (ly:music-property expr 'once)))

    (format #f "~a\\override ~{~a~^.~} = ~a~a"
            (if (or (null? once)
                    (not once))
                ""
                "\\once ")
            (if (eqv? (*current-context*) 'Bottom)
                (cons symbol properties)
                (cons* (*current-context*) symbol properties))
            (value->lily-string value)
            (new-line->lily-string))))

(define-display-method RevertProperty (expr)
  (let* ((symbol (ly:music-property expr 'symbol))
         (properties (ly:music-property expr 'grob-property-path
                                        (list (ly:music-property expr
                                                                 'grob-property))))
         (once (ly:music-property expr 'once #f)))
    (format #f "~a\\revert ~{~a~^.~}~a"
            (if once "\\once " "")
            (if (eqv? (*current-context*) 'Bottom)
                (cons symbol properties)
                (cons* (*current-context*) symbol properties))
            (new-line->lily-string))))

(define-display-method TimeSignatureMusic (expr)
  (let* ((num (ly:music-property expr 'numerator))
         (den (ly:music-property expr 'denominator))
         (structure (ly:music-property expr 'beat-structure)))
    (if (null? structure)
        (format #f
                "\\time ~a/~a~a"
                num den
                (new-line->lily-string))
        (format #f
                ;; This is silly but the latter will also work for #f
                ;; and other
                (if (key-list? structure)
                    "\\time ~{~a~^,~} ~a/~a~a"
                    "\\time #'~a ~a/~a~a")
                structure num den
                (new-line->lily-string)))))

;;; \melisma and \melismaEnd
(define-extra-display-method ContextSpeccedMusic (expr)
  "If expr is a melisma, return \"\\melisma\", otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 element (music 'PropertySet
                                                value #t
                                                symbol 'melismaBusy)))
                    "\\melisma"))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If expr is a melisma end, return \"\\melismaEnd\", otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 element (music 'PropertyUnset
                                                symbol 'melismaBusy)))
                    "\\melismaEnd"))

;;; \tempo
(define-extra-display-method SequentialMusic (expr)
  (with-music-match (expr (music 'SequentialMusic
                                 elements ((music 'TempoChangeEvent
                                                  text ?text
                                                  tempo-unit ?unit
                                                  metronome-count ?count)
                                           (music 'ContextSpeccedMusic
                                                  element (music 'PropertySet
                                                                 symbol 'tempoWholesPerMinute)))))
                    (format #f "\\tempo ~{~a~a~}~a = ~a~a"
                            (if (markup? ?text)
                                (list (markup->lily-string ?text) " ")
                                '())
                            (duration->lily-string ?unit #:force-duration #t)
                            (if (pair? ?count)
                                (format #f "~a - ~a" (car ?count) (cdr ?count))
                                ?count)
                            (new-line->lily-string))))

(define-display-method TempoChangeEvent (expr)
  (let ((text (ly:music-property expr 'text)))
    (format #f "\\tempo ~a~a"
            (markup->lily-string text)
            (new-line->lily-string))))

;;; \clef
(define clef-name-alist #f)
(define-public (memoize-clef-names clefs)
  "Initialize @code{clef-name-alist}, if not already set."
  (if (not clef-name-alist)
      (set! clef-name-alist
            (map (lambda (name+vals)
                   (cons (cdr name+vals)
                         (car name+vals)))
                 clefs))))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If @var{expr} is a clef change, return \"\\clef ...\".
Otherwise, return @code{#f}."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 context-type 'Staff
                                 element (music 'SequentialMusic
                                                elements ((music 'PropertySet
                                                                 value ?clef-glyph
                                                                 symbol 'clefGlyph)
                                                          (music 'PropertySet
                                                                 symbol 'middleCClefPosition)
                                                          (music 'PropertySet
                                                                 value ?clef-position
                                                                 symbol 'clefPosition)
                                                          (music 'PropertySet
                                                                 value ?clef-transposition
                                                                 symbol 'clefTransposition)
                                                          (music 'PropertySet
                                                                 value ?clef-transposition-style
                                                                 symbol 'clefTranspositionStyle)
                                                          (music 'ApplyContext
                                                                 procedure ly:set-middle-C!)))))
                    (let ((clef-name (assoc-get (list ?clef-glyph ?clef-position 0)
                                                clef-name-alist)))
                      (and clef-name
                           (format #f "\\clef \"~a~?\"~a"
                                   clef-name
                                   (case ?clef-transposition-style
                                     ((parenthesized) "~a(~a)")
                                     ((bracketed) "~a[~a]")
                                     (else "~a~a"))
                                   (cond ((zero? ?clef-transposition)
                                          (list "" ""))
                                         ((positive? ?clef-transposition)
                                          (list "^" (1+ ?clef-transposition)))
                                         (else (list "_" (- 1 ?clef-transposition))))
                                   (new-line->lily-string))))))

(define-display-method BarEvent (expr)
  (format #f "\\bar \"~a\"~a"
          (ly:music-property expr 'bar-type)
          (new-line->lily-string)))

(define-display-method FineEvent (expr)
  "\\fine")

(define-display-method SectionEvent (expr)
  "\\section")

;;; \partial
(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a partial measure, return \"\\partial ...\".
Otherwise, return #f."
  (with-music-match (expr (music
                           'ContextSpeccedMusic
                           context-type 'Timing
                           element (music
                                    'PartialSet
                                    duration ?duration)))

                    (and ?duration
                         (format #f "\\partial ~a"
                                 (duration->lily-string ?duration #:force-duration #t)))))

;;;
;;;

(define-display-method ApplyOutputEvent (applyoutput)
  (let ((proc (ly:music-property applyoutput 'procedure))
        (ctx  (ly:music-property applyoutput 'context-type))
        (grob (ly:music-property applyoutput 'symbol)))
    (format #f "\\applyOutput ~a~@[.~a~] #~a"
            ctx
            (and (symbol? grob) grob)
            (or (procedure-name proc)
                (with-output-to-string
                  (lambda ()
                    (pretty-print (procedure-source proc))))))))

(define-display-method ApplyContext (applycontext)
  (let ((proc (ly:music-property applycontext 'procedure)))
    (format #f "\\applyContext #~a"
            (or (procedure-name proc)
                (with-output-to-string
                  (lambda ()
                    (pretty-print (procedure-source proc))))))))

;;; \partCombine
(define-display-method PartCombineMusic (expr)
  (with-music-match
   (expr (music 'PartCombineMusic
                direction ?dir
                elements ((music 'ContextSpeccedMusic
                                 element
                                 (music 'SimultaneousMusic
                                        tags (list '$partCombine)
                                        elements (?part-one-changes
                                                  ?part-one)))
                          (music 'ContextSpeccedMusic
                                 element
                                 (music 'SimultaneousMusic
                                        tags (list '$partCombine)
                                        elements (?part-two-changes
                                                  ?part-two))))))
   (format #f "\\partCombine~a ~a~a~a"
           (cond ((equal? ?dir UP) "Up")
                 ((equal? ?dir DOWN) "Down")
                 (else ""))
           (music->lily-string ?part-one)
           (new-line->lily-string)
           (music->lily-string ?part-two))))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a \\partCombine expression, return \"\\partCombine ...\".
Otherwise, return #f."
  (with-music-match
   (expr (music 'ContextSpeccedMusic
                context-type 'Staff
                element (music 'SimultaneousMusic
                               elements ((music 'ContextSpeccedMusic
                                                context-id "one"
                                                context-type 'Voice)
                                         (music 'ContextSpeccedMusic
                                                context-id "two"
                                                context-type 'Voice)
                                         (music 'ContextSpeccedMusic
                                                context-id "shared"
                                                context-type 'Voice)
                                         (music 'ContextSpeccedMusic
                                                context-id "solo"
                                                context-type 'Voice)
                                         (music 'ContextSpeccedMusic
                                                context-id "null"
                                                context-type 'NullVoice)
                                         ?pc-music
                                         ?pc-marks))))
   (with-music-match
    (?pc-music (music 'PartCombineMusic))
    (format #f "~a" (music->lily-string ?pc-music)))))

(define-display-method UnrelativableMusic (expr)
  (music->lily-string (ly:music-property expr 'element)))

;;; Cue notes
(define-display-method QuoteMusic (expr)
  (or (with-music-match (expr (music
                               'QuoteMusic
                               quoted-voice-direction ?quoted-voice-direction
                               quoted-music-name ?quoted-music-name
                               quoted-context-id "cue"
                               quoted-context-type 'CueVoice
                               element ?music))
                        (format #f "\\cueDuring ~s #~a ~a"
                                ?quoted-music-name
                                ?quoted-voice-direction
                                (music->lily-string ?music)))
      (format #f "\\quoteDuring ~s ~a"
              (ly:music-property expr 'quoted-music-name)
              (music->lily-string (ly:music-property expr 'element)))))

;;;
;;; Breaks
;;;
(define-display-method LineBreakEvent (expr)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noBreak"
      "\\break"))

(define-display-method PageBreakEvent (expr)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noPageBreak"
      "\\pageBreak"))

(define-display-method PageTurnEvent (expr)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noPageTurn"
      "\\pageTurn"))

(define-extra-display-method EventChord (expr)
  (with-music-match (expr (music 'EventChord
                                 elements ((music 'LineBreakEvent
                                                  break-permission 'force)
                                           (music 'PageBreakEvent
                                                  break-permission 'force))))
                    "\\pageBreak"))

(define-extra-display-method EventChord (expr)
  (with-music-match (expr (music 'EventChord
                                 elements ((music 'LineBreakEvent
                                                  break-permission 'force)
                                           (music 'PageBreakEvent
                                                  break-permission 'force)
                                           (music 'PageTurnEvent
                                                  break-permission 'force))))
                    "\\pageTurn"))

(define-extra-display-method EventChord (expr)
  (with-music-match (expr (music 'EventChord
                                 page-marker #t
                                 page-label ?lab
                                 elements ((music 'LabelEvent
                                                  page-label ?lab-again))))
                    (string-append "\\label " (value->lily-string ?lab))))

(define-extra-display-method EventChord (expr)
  (with-music-match (expr (music 'EventChord
                                 elements ((music 'SpacingSectionEvent))))
                    "\\newSpacingSection"))

;;;
;;; Lyrics
;;;

;;; \lyricsto
(define-display-method LyricCombineMusic (expr)
  (format #f "\\lyricsto ~s ~a"
          (ly:music-property expr 'associated-context)
          (parameterize ((*explicit-mode* #f)
                         (*omit-duration* #t))
            (music->lily-string (ly:music-property expr 'element)))))

;; \autoChange
(define-extra-display-method SimultaneousMusic (expr)
  (with-music-match
   (expr (music 'SimultaneousMusic
                elements ((music 'ContextSpeccedMusic
                                 context-id "up"
                                 context-type 'Staff
                                 element ?ac-music)
                          (music 'ContextSpeccedMusic
                                 context-id "up"
                                 context-type 'Staff)
                          (music 'ContextSpeccedMusic
                                 context-id "down"
                                 context-type 'Staff))))
   (with-music-match
    (?ac-music (music 'ContextSpeccedMusic
                      element (music 'SimultaneousMusic
                                     tags (list '$autoChange)
                                     elements (?changes
                                               ?inner-music))))
    (format #f "\\autoChange ~a" (music->lily-string ?inner-music)))))

;; \addlyrics
(define-extra-display-method SimultaneousMusic (expr)
  (with-music-match (expr (music 'SimultaneousMusic
                                 elements ((music 'ContextSpeccedMusic
                                                  context-id ?id
                                                  context-type 'Voice
                                                  element ?note-sequence)
                                           (music 'ContextSpeccedMusic
                                                  context-type 'Lyrics
                                                  create-new #t
                                                  element (music 'LyricCombineMusic
                                                                 associated-context ?associated-id
                                                                 element ?lyric-sequence)))))
                    (if (string=? ?id ?associated-id)
                        (format #f "~a~a \\addlyrics ~a"
                                (music->lily-string ?note-sequence)
                                (new-line->lily-string)
                                (parameterize ((*explicit-mode* #f)
                                               (*omit-duration* #t))
                                  (music->lily-string ?lyric-sequence)))
                        #f)))

;; Silence internal event sent at end of each lyrics block
(define-display-method CompletizeExtenderEvent (expr)
  "")


(define-display-method StaffHighlightEvent (event)
  (if (eqv? LEFT (ly:music-property event 'span-direction))
      (let ((color (ly:music-property event 'color)))
        ;; TODO: for #(rgb-color r g b) calls, this will convert to
        ;; #'(r g b).  Is this fine?  Do we want to make rgb-color
        ;; return an opaque object instead?
        (string-append "\\staffHighlight " (value->lily-string color)))
      "\\stopStaffHighlight"))

(define-span-event-display-method MeasureCounterEvent (_) #f
  "\\startMeasureCount" "\\stopMeasureCount")

(define-span-event-display-method MeasureSpannerEvent (_) #f
  "\\startMeasureSpanner" "\\stopMeasureSpanner")

(define-display-method AnnotateOutputEvent (event)
  (let* ((symbol (ly:music-property event 'symbol #f))
         (X-offset (ly:music-property event 'X-offset))
         (Y-offset (ly:music-property event 'Y-offset))
         (text (ly:music-property event 'text)))
    (if symbol
        (format #f "\\balloonGrobText ~a #'(~a . ~a) ~a"
                (value->lily-string symbol)
                X-offset
                Y-offset
                (value->lily-string text))
        (format #f "\\balloonText ~a #'(~a . ~a)"
                X-offset
                Y-offset
                (value->lily-string text)))))


(define-extra-display-method EventChord (expr)
  (with-music-match (expr (music 'EventChord
                                 elements (?mus)))
                    (and (eq? 'AnnotateOutputEvent (ly:music-property ?mus 'name))
                         ;; \balloonGrobText attaches an AnnotateOutputEvent to
                         ;; an empty chord, drop that empty chord.
                         (value->lily-string ?mus))))

(define-display-method PostEvents (expr)
  (string-join
   (map music->lily-string (ly:music-property expr 'elements))
   ""))
