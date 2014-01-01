;;; define-music-display-methods.scm -- data for displaying music
;;; expressions using LilyPond notation.
;;;
;;; Copyright (C) 2005--2012 Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display method implementation
;;;

(define-module (scm display-lily))

;;;
;;; Scheme forms
;;;
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

;; It is a pity that there is no rassoc in Scheme.
(define* (rassoc item alist #:optional (test equal?))
  (do ((alist alist (cdr alist))
       (result #f result))
      ((or result (null? alist)) result)
    (if (and (car alist) (test item (cdar alist)))
        (set! result (car alist)))))

(define-public (note-name->lily-string ly-pitch parser)
  ;; here we define a custom pitch= function, since we do not want to
  ;; test whether octaves are also equal. (otherwise, we would be using equal?)
  (define (pitch= pitch1 pitch2)
    (and (= (ly:pitch-notename pitch1) (ly:pitch-notename pitch2))
         (= (ly:pitch-alteration pitch1) (ly:pitch-alteration pitch2))))
  (let ((result (rassoc ly-pitch (ly:parser-lookup parser 'pitchnames) pitch=)))
    (if result
        (car result)
        #f)))

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
(define*-public (duration->lily-string ly-duration #:key (prev-duration (*previous-duration*))
                                       (force-duration (*force-duration*))
                                       (time-scale (*time-scale*))
                                       remember)
  (if remember (*previous-duration* ly-duration))
  (let ((log2    (ly:duration-log ly-duration))
        (dots    (ly:duration-dot-count ly-duration))
        (scale (ly:duration-scale ly-duration)))
    (if (or force-duration (not prev-duration) (not (equal? ly-duration prev-duration)))
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

(define (post-event? m)
  (music-is-of-type? m 'post-event))

(define* (event-direction->lily-string event #:optional (required #t))
  (let ((direction (ly:music-property event 'direction)))
    (cond ((or (not direction) (null? direction) (= CENTER direction))
           (if required "-" ""))
          ((= UP direction) "^")
          ((= DOWN direction) "_")
          (else ""))))

(define-macro (define-post-event-display-method type vars direction-required str)
  `(define-display-method ,type ,vars
     (format #f "~a~a"
             (event-direction->lily-string ,(car vars) ,direction-required)
             ,str)))

(define-macro (define-span-event-display-method type vars direction-required str-start str-stop)
  `(define-display-method ,type ,vars
     (format #f "~a~a"
             (event-direction->lily-string ,(car vars) ,direction-required)
             (if (= START (ly:music-property ,(car vars) 'span-direction))
                 ,str-start
                 ,str-stop))))

(define-display-method HyphenEvent (event parser)
  " --")
(define-display-method ExtenderEvent (event parser)
  " __")
(define-display-method TieEvent (event parser)
  " ~")
(define-display-method BeamForbidEvent (event parser)
  "\\noBeam")
(define-display-method StringNumberEvent (event parser)
  (format #f "\\~a" (ly:music-property event 'string-number)))


(define-display-method TremoloEvent (event parser)
  (let ((tremolo-type (ly:music-property event 'tremolo-type)))
    (format #f ":~a" (if (= 0 tremolo-type)
                         ""
                         tremolo-type))))

(define-display-method ArticulationEvent (event parser) #t
  (let* ((articulation  (ly:music-property event 'articulation-type))
         (shorthand
          (case (string->symbol articulation)
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

(define-post-event-display-method FingeringEvent (event parser) #t
  (ly:music-property event 'digit))

(define-post-event-display-method TextScriptEvent (event parser) #t
  (markup->lily-string (ly:music-property event 'text)))

(define-post-event-display-method MultiMeasureTextEvent (event parser) #t
  (markup->lily-string (ly:music-property event 'text)))

(define-post-event-display-method BendAfterEvent (event parser) #f
  (format #f "\\bendAfter #~a " (ly:music-property event 'delta-step)))

(define-post-event-display-method HarmonicEvent (event parser) #f "\\harmonic")
(define-post-event-display-method GlissandoEvent (event parser) #f "\\glissando")
(define-post-event-display-method ArpeggioEvent (event parser) #f "\\arpeggio")
(define-post-event-display-method AbsoluteDynamicEvent (event parser) #f
  (format #f "\\~a" (ly:music-property event 'text)))

(define-post-event-display-method StrokeFingerEvent (event parser) #f
  (format #f "\\rightHandFinger #~a " (ly:music-property event 'digit)))

(define-span-event-display-method BeamEvent (event parser) #f "[" "]")
(define-span-event-display-method SlurEvent (event parser) #f "(" ")")
(define-span-event-display-method CrescendoEvent (event parser) #f "\\<" "\\!")
(define-span-event-display-method DecrescendoEvent (event parser) #f "\\>" "\\!")
(define-span-event-display-method EpisemaEvent (event parser) #f "\\episemInitium" "\\episemFinis")
(define-span-event-display-method PhrasingSlurEvent (event parser) #f "\\(" "\\)")
(define-span-event-display-method SustainEvent (event parser) #f "\\sustainOn" "\\sustainOff")
(define-span-event-display-method SostenutoEvent (event parser) #f "\\sostenutoOn" "\\sostenutoOff")
(define-span-event-display-method TextSpanEvent (event parser) #f "\\startTextSpan" "\\stopTextSpan")
(define-span-event-display-method TrillSpanEvent (event parser) #f "\\startTrillSpan" "\\stopTrillSpan")
(define-span-event-display-method StaffSpanEvent (event parser) #f "\\startStaff" "\\stopStaff")
(define-span-event-display-method NoteGroupingEvent (event parser) #f "\\startGroup" "\\stopGroup")
(define-span-event-display-method UnaCordaEvent (event parser) #f "\\unaCorda" "\\treCorde")

;;;
;;; Graces
;;;

(define-display-method GraceMusic (expr parser)
  (format #f "\\grace ~a"
          (music->lily-string (ly:music-property expr 'element) parser)))

;; \acciaccatura \appoggiatura \grace
;; TODO: it would be better to compare ?start and ?stop
;; with startAppoggiaturaMusic and stopAppoggiaturaMusic,
;; using a custom music equality predicate.
(define-extra-display-method GraceMusic (expr parser)
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
                                           (format #f "\\appoggiatura ~a" (music->lily-string ?music parser))))))


(define-extra-display-method GraceMusic (expr parser)
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
                                           (format #f "\\acciaccatura ~a" (music->lily-string ?music parser))))))

(define-extra-display-method GraceMusic (expr parser)
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
                         (format #f "\\grace ~a" (music->lily-string ?music parser)))))

;;;
;;; Music sequences
;;;

(define-display-method SequentialMusic (seq parser)
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
                                          (music->lily-string music parser))
                                        elements))
            (if force-line-break 1 0)
            (if force-line-break (*indent*) 1))))

(define-display-method SimultaneousMusic (sim parser)
  (parameterize ((*indent* (+ 3 (*indent*))))
                (format #f "<< ~{~a ~}>>"
                        (map-in-order (lambda (music)
                                        (music->lily-string music parser))
                                      (ly:music-property sim 'elements)))))

(define-extra-display-method SimultaneousMusic (expr parser)
  "If `sim' is an \afterGrace expression, return \"\\afterGrace ...\".
Otherwise, return #f."
  ;; TODO: do something with afterGraceFraction?
  (with-music-match (expr (music 'SimultaneousMusic
                                 elements (?before-grace
                                           (music 'SequentialMusic
                                                  elements ((music 'SkipMusic)
                                                            (music 'GraceMusic
                                                                   element ?grace))))))
                    (format #f "\\afterGrace ~a ~a"
                            (music->lily-string ?before-grace parser)
                            (music->lily-string ?grace parser))))

;;;
;;; Chords
;;;

(define-display-method EventChord (chord parser)
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
          (partition (lambda (m) (music-is-of-type? m 'rhythmic-event))
                     elements))
      (lambda (chord-elements other-elements)
        (cond ((pair? chord-elements)
               ;; note_chord_element :
               ;; '<' (notepitch | drumpitch)* '>" duration post_events
               (let ((duration (duration->lily-string (ly:music-property
                                                       (car chord-elements)
                                                       'duration)
                                                      #:remember #t)))
                 ;; Format duration first so that it does not appear on
                 ;; chord elements
                 (format #f "< ~{~a ~}>~a~:{~:[-~;~]~a~^ ~}"
                         (map-in-order (lambda (music)
                                         (music->lily-string music parser))
                                       chord-elements)
                         duration
                         (map-in-order (lambda (music)
                                         (list
                                          (post-event? music)
                                          (music->lily-string music parser)))
                                       other-elements))))
              ((ly:duration? chord-repeat)
               (let ((duration (duration->lily-string chord-repeat
                                                      #:remember #t)))
                 (format #f "q~a~:{~:[-~;~]~a~^ ~}"
                         duration
                         (map-in-order (lambda (music)
                                         (list
                                          (post-event? music)
                                          (music->lily-string music parser)))
                                       other-elements))))

              ((and (= 1 (length other-elements))
                    (not (post-event? (car other-elements))))
               (format #f (music->lily-string (car other-elements) parser)))
              (else
               (format #f "< >~:{~:[-~;~]~a~^ ~}"
                       (map-in-order (lambda (music)
                                       (list
                                        (post-event? music)
                                        (music->lily-string music parser)))
                                     other-elements))))))))

(define-display-method MultiMeasureRestMusic (mmrest parser)
  (format #f "R~a~{~a~^ ~}"
          (duration->lily-string (ly:music-property mmrest 'duration)
                                 #:remember #t)
          (map-in-order (lambda (music)
                          (music->lily-string music parser))
                        (ly:music-property mmrest 'articulations))))

(define-display-method SkipMusic (skip parser)
  (format #f "\\skip ~a" (duration->lily-string (ly:music-property skip 'duration) #:force-duration #t)))

(define-display-method OttavaMusic (ottava parser)
  (format #f "\\ottava #~a" (ly:music-property ottava 'ottava-number)))

;;;
;;; Notes, rests, skips...
;;;

(define (simple-note->lily-string event parser)
  (format #f "~a~a~a~a~a~a~:{~:[-~;~]~a~}" ; pitchname octave !? octave-check duration optional_rest articulations
          (note-name->lily-string (ly:music-property event 'pitch) parser)
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
          (duration->lily-string (ly:music-property event 'duration)
                                 #:remember #t)
          (if ((make-music-type-predicate 'RestEvent) event)
              "\\rest" "")
          (map-in-order (lambda (event)
                          (list
                           (post-event? event)
                           (music->lily-string event parser)))
                        (ly:music-property event 'articulations))))

(define-display-method NoteEvent (note parser)
  (cond ((not (null? (ly:music-property note 'pitch))) ;; note
         (simple-note->lily-string note parser))
        ((not (null? (ly:music-property note 'drum-type))) ;; drum
         (format #f "~a~a~{~a~}" (ly:music-property note 'drum-type)
                 (duration->lily-string (ly:music-property note 'duration)
                                        #:remember #t)
                 (map-in-order (lambda (event)
                                 (music->lily-string event parser))
                               (ly:music-property note 'articulations))))
        (else ;; unknown?
         "")))

(define-display-method ClusterNoteEvent (note parser)
  (simple-note->lily-string note parser))

(define-display-method RestEvent (rest parser)
  (if (not (null? (ly:music-property rest 'pitch)))
      (simple-note->lily-string rest parser)
      (format #f "r~a~{~a~}"
              (duration->lily-string (ly:music-property rest 'duration)
                                     #:remember #t)
              (map-in-order (lambda (event)
                              (music->lily-string event parser))
                            (ly:music-property rest 'articulations)))))

(define-display-method MultiMeasureRestEvent (rest parser)
  (string-append "R" (duration->lily-string (ly:music-property rest 'duration)
                                            #:remember #t)))

(define-display-method SkipEvent (rest parser)
  (format #f "s~a~{~a~}"
          (duration->lily-string (ly:music-property rest 'duration)
                                 #:remember #t)
          (map-in-order (lambda (event)
                          (music->lily-string event parser))
                        (ly:music-property rest 'articulations))))

(define-display-method RepeatedChord (chord parser)
  (music->lily-string (ly:music-property chord 'element) parser))

(define-display-method MarkEvent (mark parser)
  (let ((label (ly:music-property mark 'label)))
    (if (null? label)
        "\\mark \\default"
        (format #f "\\mark ~a" (markup->lily-string label)))))

(define-display-method KeyChangeEvent (key parser)
  (let ((pitch-alist (ly:music-property key 'pitch-alist))
        (tonic (ly:music-property key 'tonic)))
    (if (or (null? pitch-alist)
            (null? tonic))
        "\\key \\default"
        (let ((c-pitch-alist (ly:transpose-key-alist pitch-alist
                                                     (ly:pitch-diff (ly:make-pitch 0 0 0) tonic))))
          (format #f "\\key ~a \\~a~a"
                  (note-name->lily-string (ly:music-property key 'tonic) parser)
                  (any (lambda (mode)
                         (if (and parser
                                  (equal? (ly:parser-lookup parser mode) c-pitch-alist))
                             (symbol->string mode)
                             #f))
                       '(major minor ionian locrian aeolian mixolydian lydian phrygian dorian))
                  (new-line->lily-string))))))

(define-display-method RelativeOctaveCheck (octave parser)
  (let ((pitch (ly:music-property octave 'pitch)))
    (format #f "\\octaveCheck ~a~a"
            (note-name->lily-string pitch parser)
            (octave->lily-string pitch))))

(define-display-method VoiceSeparator (sep parser)
  "\\\\")

(define-display-method LigatureEvent (ligature parser)
  (if (= START (ly:music-property ligature 'span-direction))
      "\\["
      "\\]"))

(define-display-method BarCheck (check parser)
  (format #f "|~a" (new-line->lily-string)))

(define-display-method PesOrFlexaEvent (expr parser)
  "\\~")

(define-display-method BassFigureEvent (figure parser)
  (let ((alteration (ly:music-property figure 'alteration))
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
                (cond
                 ((= alteration DOUBLE-FLAT) "--")
                 ((= alteration FLAT) "-")
                 ((= alteration NATURAL) "!")
                 ((= alteration SHARP) "+")
                 ((= alteration DOUBLE-SHARP) "++")
                 (else "")))
            (if (null? bracket-stop) "" "]"))))

(define-display-method LyricEvent (lyric parser)
  (format "~a~{~a~^ ~}"
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
          (map-in-order (lambda (m) (music->lily-string m parser))
                        (ly:music-property lyric 'articulations))))

(define-display-method BreathingEvent (event parser)
  "\\breathe")

;;;
;;; Staff switches
;;;

(define-display-method AutoChangeMusic (m parser)
  (format #f "\\autochange ~a"
          (music->lily-string (ly:music-property m 'element) parser)))

(define-display-method ContextChange (m parser)
  (format #f "\\change ~a = \"~a\""
          (ly:music-property m 'change-to-type)
          (ly:music-property m 'change-to-id)))

;;;

(define-display-method TimeScaledMusic (times parser)
  (let* ((num (ly:music-property times 'numerator))
         (den (ly:music-property times 'denominator))
         (span (ly:music-property times 'duration #f))
         ;; need to format before changing time scale
         (formatted-span
          (and span (duration->lily-string span #:force-duration #t)))
         (scale (/ num den))
         (time-scale (*time-scale*)))
    (*previous-duration* #f)
    (let ((result
           (parameterize ((*force-line-break* #f)
                          (*time-scale* (* time-scale scale)))
                         (format #f "\\tuplet ~a/~a ~@[~a ~]~a"
                                 den
                                 num
                                 formatted-span
                                 (music->lily-string (ly:music-property times 'element) parser)))))
      (*previous-duration* #f)
      result)))

(define-display-method RelativeOctaveMusic (m parser)
  (music->lily-string (ly:music-property m 'element) parser))

(define-display-method TransposedMusic (m parser)
  (music->lily-string (ly:music-property m 'element) parser))

;;;
;;; Repeats
;;;

(define-display-method AlternativeEvent (alternative parser) "")

(define (repeat->lily-string expr repeat-type parser)
  (let* ((main (music->lily-string (ly:music-property expr 'element) parser)))
    (format #f "\\repeat ~a ~a ~a ~a"
            repeat-type
            (ly:music-property expr 'repeat-count)
            main
            (let ((alternatives (ly:music-property expr 'elements)))
              (if (null? alternatives)
                  ""
                  (format #f "\\alternative { ~{~a ~}}"
                          (map-in-order (lambda (music)
                                          (music->lily-string music parser))
                                        alternatives)))))))

(define-display-method VoltaRepeatedMusic (expr parser)
  (repeat->lily-string expr "volta" parser))

(define-display-method UnfoldedRepeatedMusic (expr parser)
  (repeat->lily-string expr "unfold" parser))

(define-display-method PercentRepeatedMusic (expr parser)
  (repeat->lily-string expr "percent" parser))

(define-display-method TremoloRepeatedMusic (expr parser)
  (let* ((main (ly:music-property expr 'element))
         (children (if (music-is-of-type? main 'sequential-music)
                       ;; \repeat tremolo n { ... }
                       (length (extract-named-music main '(EventChord
                                                           NoteEvent)))
                       ;; \repeat tremolo n c4
                       1))
         (times (ly:music-property expr 'repeat-count))

         ;; # of dots is equal to the 1 in bitwise representation (minus 1)!
         (dots (1- (logcount (* times children))))
         ;; The remaining missing multiplicator to scale the notes by
         ;; times * children
         (mult (/ (* times children (ash 1 dots)) (1- (ash 2 dots))))
         (shift (- (ly:intlog2 (floor mult)))))
    (set! main (ly:music-deep-copy main))
    ;; Adjust the time of the notes
    (ly:music-compress main (ly:make-moment children 1))
    ;; Adjust the displayed note durations
    (shift-duration-log main (- shift) (- dots))
    (format #f "\\repeat tremolo ~a ~a"
            times
            (music->lily-string main parser))))

;;;
;;; Contexts
;;;

(define-display-method ContextSpeccedMusic (expr parser)
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
                          (music->lily-string music parser)))))

;; special cases: \figures \lyrics \drums
(define-extra-display-method ContextSpeccedMusic (expr parser)
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 create-new #t
                                 property-operations ?op
                                 context-type ?context-type
                                 element ?sequence))
                    (if (null? ?op)
                        (parameterize ((*explicit-mode* #f))
                                      (case ?context-type
                                        ((FiguredBass)
                                         (format #f "\\figures ~a" (music->lily-string ?sequence parser)))
                                        ((Lyrics)
                                         (format #f "\\lyrics ~a" (music->lily-string ?sequence parser)))
                                        ((DrumStaff)
                                         (format #f "\\drums ~a" (music->lily-string ?sequence parser)))
                                        (else
                                         #f)))
                        #f)))

;;; Context properties

(define-extra-display-method ContextSpeccedMusic (expr parser)
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
                      (music->lily-string element parser))
        #f)))

(define-public (value->lily-string arg parser)
  (cond ((ly:music? arg)
         (music->lily-string arg parser))
        ((string? arg)
         (format #f "#~s" arg))
        ((markup? arg)
         (markup->lily-string arg))
        ((ly:duration? arg)
         (format #f "##{ ~a #}" (duration->lily-string arg #:force-duration #t)))
        ((ly:pitch? arg)
         (format #f "~a~a"
                 (note-name->lily-string arg parser)
                 (octave->lily-string arg)))
        (else
         (format #f "#~a" (scheme-expr->lily-string arg)))))

(define-display-method PropertySet (expr parser)
  (let ((property (ly:music-property expr 'symbol))
        (value (ly:music-property expr 'value))
        (once (ly:music-property expr 'once)))
    (format #f "~a\\set ~a~a = ~a~a"
            (if (and (not (null? once)))
                "\\once "
                "")
            (if (eqv? (*current-context*) 'Bottom)
                ""
                (format #f "~a . " (*current-context*)))
            property
            (value->lily-string value parser)
            (new-line->lily-string))))

(define-display-method PropertyUnset (expr parser)
  (format #f "\\unset ~a~a~a"
          (if (eqv? (*current-context*) 'Bottom)
              ""
              (format #f "~a . " (*current-context*)))
          (ly:music-property expr 'symbol)
          (new-line->lily-string)))

;;; Layout properties

(define-display-method OverrideProperty (expr parser)
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
            (value->lily-string value parser)
            (new-line->lily-string))))

(define-display-method RevertProperty (expr parser)
  (let* ((symbol (ly:music-property expr 'symbol))
         (properties (ly:music-property expr 'grob-property-path
                                        (list (ly:music-property expr 'grob-property)))))
    (format #f "\\revert ~{~a~^.~}~a"
            (if (eqv? (*current-context*) 'Bottom)
                (cons symbol properties)
                (cons* (*current-context*) symbol properties))
            (new-line->lily-string))))

(define-display-method TimeSignatureMusic (expr parser)
  (let* ((num (ly:music-property expr 'numerator))
         (den (ly:music-property expr 'denominator))
         (structure (ly:music-property expr 'beat-structure)))
    (if (null? structure)
        (format #f
                "\\time ~a/~a~a"
                num den
                (new-line->lily-string))
        (format #f
                "\\time #'~a ~a/~a~a"
                structure num den
                (new-line->lily-string)))))

;;; \melisma and \melismaEnd
(define-extra-display-method ContextSpeccedMusic (expr parser)
  "If expr is a melisma, return \"\\melisma\", otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 element (music 'PropertySet
                                                value #t
                                                symbol 'melismaBusy)))
                    "\\melisma"))

(define-extra-display-method ContextSpeccedMusic (expr parser)
  "If expr is a melisma end, return \"\\melismaEnd\", otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 element (music 'PropertyUnset
                                                symbol 'melismaBusy)))
                    "\\melismaEnd"))

;;; \tempo
(define-extra-display-method SequentialMusic (expr parser)
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

(define-display-method TempoChangeEvent (expr parser)
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

(define-extra-display-method ContextSpeccedMusic (expr parser)
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
                                                          (music 'ApplyContext
                                                                 procedure ly:set-middle-C!)))))
                    (let ((clef-name (assoc-get (list ?clef-glyph ?clef-position 0)
                                                clef-name-alist)))
                      (if clef-name
                          (format #f "\\clef \"~a~{~a~a~}\"~a"
                                  clef-name
                                  (cond ((= 0 ?clef-transposition)
                                         (list "" ""))
                                        ((> ?clef-transposition 0)
                                         (list "^" (1+ ?clef-transposition)))
                                        (else
                                         (list "_" (- 1 ?clef-transposition))))
                                  (new-line->lily-string))
                          #f))))

;;; \bar
(define-extra-display-method ContextSpeccedMusic (expr parser)
  "If `expr' is a bar, return \"\\bar ...\".
Otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 context-type 'Timing
                                 element (music 'PropertySet
                                                value ?bar-type
                                                symbol 'whichBar)))
                    (format #f "\\bar \"~a\"~a" ?bar-type (new-line->lily-string))))

;;; \partial
(define-extra-display-method ContextSpeccedMusic (expr parser)
  "If `expr' is a partial measure, return \"\\partial ...\".
Otherwise, return #f."
  (with-music-match (expr (music
                           'ContextSpeccedMusic
                           element (music
                                    'ContextSpeccedMusic
                                    context-type 'Timing
                                    element (music
                                             'PartialSet
                                             duration ?duration))))

                    (and ?duration
                         (format #f "\\partial ~a"
                                 (duration->lily-string ?duration #:force-duration #t)))))

;;;
;;;

(define-display-method ApplyOutputEvent (applyoutput parser)
  (let ((proc (ly:music-property applyoutput 'procedure))
        (ctx  (ly:music-property applyoutput 'context-type)))
    (format #f "\\applyOutput #'~a #~a"
            ctx
            (or (procedure-name proc)
                (with-output-to-string
                  (lambda ()
                    (pretty-print (procedure-source proc))))))))

(define-display-method ApplyContext (applycontext parser)
  (let ((proc (ly:music-property applycontext 'procedure)))
    (format #f "\\applyContext #~a"
            (or (procedure-name proc)
                (with-output-to-string
                  (lambda ()
                    (pretty-print (procedure-source proc))))))))

;;; \partcombine
(define-display-method PartCombineMusic (expr parser)
  (format #f "\\partcombine ~{~a ~}"
          (map-in-order (lambda (music)
                          (music->lily-string music parser))
                        (ly:music-property expr 'elements))))

(define-extra-display-method PartCombineMusic (expr parser)
  (with-music-match (expr (music 'PartCombineMusic
                                 elements ((music 'UnrelativableMusic
                                                  element (music 'ContextSpeccedMusic
                                                                 context-id "one"
                                                                 context-type 'Voice
                                                                 element ?sequence1))
                                           (music 'UnrelativableMusic
                                                  element (music 'ContextSpeccedMusic
                                                                 context-id "two"
                                                                 context-type 'Voice
                                                                 element ?sequence2)))))
                    (format #f "\\partcombine ~a~a~a"
                            (music->lily-string ?sequence1 parser)
                            (new-line->lily-string)
                            (music->lily-string ?sequence2 parser))))

(define-display-method UnrelativableMusic (expr parser)
  (music->lily-string (ly:music-property expr 'element) parser))

;;; Cue notes
(define-display-method QuoteMusic (expr parser)
  (or (with-music-match (expr (music
                               'QuoteMusic
                               quoted-voice-direction ?quoted-voice-direction
                               quoted-music-name ?quoted-music-name
                               quoted-context-id "cue"
                               quoted-context-type 'CueVoice
                               element ?music))
                        (format #f "\\cueDuring #~s #~a ~a"
                                ?quoted-music-name
                                ?quoted-voice-direction
                                (music->lily-string ?music parser)))
      (format #f "\\quoteDuring #~s ~a"
              (ly:music-property expr 'quoted-music-name)
              (music->lily-string (ly:music-property expr 'element) parser))))

;;;
;;; Breaks
;;;
(define-display-method LineBreakEvent (expr parser)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noBreak"
      "\\break"))

(define-display-method PageBreakEvent (expr parser)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noPageBreak"
      "\\pageBreak"))

(define-display-method PageTurnEvent (expr parser)
  (if (null? (ly:music-property expr 'break-permission))
      "\\noPageTurn"
      "\\pageTurn"))

(define-extra-display-method EventChord (expr parser)
  (with-music-match (expr (music 'EventChord
                                 elements ((music 'LineBreakEvent
                                                  break-permission 'force)
                                           (music 'PageBreakEvent
                                                  break-permission 'force))))
                    "\\pageBreak"))

(define-extra-display-method EventChord (expr parser)
  (with-music-match (expr (music 'EventChord
                                 elements ((music 'LineBreakEvent
                                                  break-permission 'force)
                                           (music 'PageBreakEvent
                                                  break-permission 'force)
                                           (music 'PageTurnEvent
                                                  break-permission 'force))))
                    "\\pageTurn"))

;;;
;;; Lyrics
;;;

;;; \lyricsto
(define-display-method LyricCombineMusic (expr parser)
  (format #f "\\lyricsto ~s ~a"
          (ly:music-property expr 'associated-context)
          (parameterize ((*explicit-mode* #f))
                        (music->lily-string (ly:music-property expr 'element) parser))))

;; \addlyrics
(define-extra-display-method SimultaneousMusic (expr parser)
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
                                (music->lily-string ?note-sequence parser)
                                (new-line->lily-string)
                                (parameterize ((*explicit-mode* #f))
                                              (music->lily-string ?lyric-sequence parser)))
                        #f)))

;; Silence internal event sent at end of each lyrics block
(define-display-method CompletizeExtenderEvent (expr parser)
  "")
