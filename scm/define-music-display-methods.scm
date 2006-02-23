;;; define-music-display-methods.scm -- data for displaying music
;;; expressions using LilyPond notation.
;;;
;;; (c) 2005--2006 Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display method implementation
;;;

(define-module (scm display-lily))

;;; `display-lily-init' must be called before using `display-lily-music'. It
;;; takes a parser object as an argument.
(define-public (display-lily-init parser)
  (*parser* parser)
  #t)

;;;
;;; Scheme forms
;;;
(define (scheme-expr->lily-string scm-arg)
  (cond ((or (number? scm-arg)
	     (string? scm-arg))
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
  "Return a string describing, in LilyPond syntax, the given markup expression."
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
	  (else		 ;; a scheme argument
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

(define (note-name->lily-string ly-pitch)
  ;; here we define a custom pitch= function, since we do not want to
  ;; test whether octaves are also equal. (otherwise, we would be using equal?)
  (define (pitch= pitch1 pitch2)
    (and (= (ly:pitch-notename pitch1) (ly:pitch-notename pitch2))
	 (= (ly:pitch-alteration pitch1) (ly:pitch-alteration pitch2))))
  (let ((result (rassoc ly-pitch (ly:parser-lookup (*parser*) 'pitchnames) pitch=)))
    (if result
	(car result)
	#f)))

(define (octave->lily-string pitch)
  (let ((octave (ly:pitch-octave pitch)))
    (cond ((>= octave 0)
	   (make-string (1+ octave) #\'))
	  ((< octave -1)
	   (make-string (1- (* -1 octave)) #\,))
	  (else ""))))

;;;
;;; durations
;;;
(define* (duration->lily-string ly-duration #:key (prev-duration (*previous-duration*))
			(force-duration (*force-duration*))
			(time-factor-numerator (*time-factor-numerator*))
			(time-factor-denominator (*time-factor-denominator*)))
  (let ((log2	 (ly:duration-log ly-duration))
	(dots	 (ly:duration-dot-count ly-duration))
	(num+den (ly:duration-factor ly-duration)))
    (if (or force-duration (not prev-duration) (not (equal? ly-duration prev-duration)))
	(string-append (case log2
			 ((-1) "\\breve")
			 ((-2) "\\longa")
			 ((-3) "\\maxima")
			 (else (number->string (expt 2 log2))))
		       (make-string dots #\.)
		       (let ((num? (not (or (= 1 (car num+den))
					    (and time-factor-numerator
						 (= (car num+den) time-factor-numerator)))))
			     (den? (not (or (= 1 (cdr num+den))
					    (and time-factor-denominator
						 (= (cdr num+den) time-factor-denominator))))))
			 (cond (den?
				(format #f "*~a/~a" (car num+den) (cdr num+den)))
			       (num?
				(format #f "*~a" (car num+den)))
			       (else ""))))
	"")))

;;;
;;; post events
;;;

(define post-event? (make-music-type-predicate	
		     'StringNumberEvent
		     'ArticulationEvent
		     'FingerEvent
		     'TextScriptEvent
		     'MultiMeasureTextEvent
		     'HyphenEvent
		     'ExtenderEvent
		     'BeamEvent
		     'SlurEvent
		     'TieEvent
		     'CrescendoEvent
		     'DecrescendoEvent
		     'PhrasingSlurEvent
		     'TremoloEvent
		     'SustainEvent
		     'SostenutoEvent
		     'ManualMelismaEvent
		     'TextSpanEvent
		     'HarmonicEvent
		     'BeamForbidEvent
		     'AbsoluteDynamicEvent
		     'TrillSpanEvent
		     'GlissandoEvent
		     'ArpeggioEvent
		     'NoteGroupingEvent
		     'UnaCordaEvent))

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

(define-display-method HyphenEvent (event)
  " --")
(define-display-method ExtenderEvent (event)
  " __")
(define-display-method TieEvent (event)
  " ~")
(define-display-method BeamForbidEvent (event)
  "\\noBeam")
(define-display-method StringNumberEvent (event)
  (format #f "\\~a" (ly:music-property event 'string-number)))


(define-display-method TremoloEvent (event)
  (let ((tremolo-type (ly:music-property event 'tremolo-type)))
    (format #f ":~a" (if (= 0 tremolo-type)
			 ""
			 tremolo-type))))

(define-post-event-display-method ArticulationEvent (event) #t
  (let ((articulation  (ly:music-property event 'articulation-type)))
    (case (string->symbol articulation)
      ((marcato) "^")
      ((stopped) "+")
      ((tenuto)	 "-")
      ((staccatissimo) "|")
      ((accent) ">")
      ((staccato) ".")
      ((portato) "_")
      (else (format #f "\\~a" articulation)))))

(define-post-event-display-method FingerEvent (event) #t
  (ly:music-property event 'digit))

(define-post-event-display-method TextScriptEvent (event) #t
  (markup->lily-string (ly:music-property event 'text)))

(define-post-event-display-method MultiMeasureTextEvent (event) #t
  (markup->lily-string (ly:music-property event 'text)))

(define-post-event-display-method HarmonicEvent (event) #t "\\harmonic")
(define-post-event-display-method GlissandoEvent (event) #t "\\glissando")
(define-post-event-display-method ArpeggioEvent (event) #t "\\arpeggio")
(define-post-event-display-method AbsoluteDynamicEvent (event) #f
  (format #f "\\~a" (ly:music-property event 'text)))

(define-span-event-display-method BeamEvent (event) #f "[" "]")
(define-span-event-display-method SlurEvent (event) #f "(" ")")
(define-span-event-display-method CrescendoEvent (event) #f "\\<" "\\!")
(define-span-event-display-method DecrescendoEvent (event) #f "\\>" "\\!")
(define-span-event-display-method PhrasingSlurEvent (event) #f "\\(" "\\)")
(define-span-event-display-method SustainEvent (event) #f "\\sustainDown" "\\sustainUp")
(define-span-event-display-method SostenutoEvent (event) #f "\\sostenutoDown" "\\sostenutoUp")
(define-span-event-display-method ManualMelismaEvent (event) #f "\\melisma" "\\melismaEnd")
(define-span-event-display-method TextSpanEvent (event) #f "\\startTextSpan" "\\stopTextSpan")
(define-span-event-display-method TrillSpanEvent (event) #f "\\startTrillSpan" "\\stopTrillSpan")
(define-span-event-display-method StaffSpanEvent (event) #f "\\startStaff" "\\stopStaff")
(define-span-event-display-method NoteGroupingEvent (event) #f "\\startGroup" "\\stopGroup")
(define-span-event-display-method UnaCordaEvent (event) #f "\\unaCorda" "\\treCorde")

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
					       elements ((music
							  'SkipEvent
							  duration (ly:make-duration 0 0 0 1))
							 (music
							  'SlurEvent
							  span-direction START))))))
			   #t)
	  (with-music-match (?stop (music 
				    'SequentialMusic
				    elements ((music
					       'EventChord
					       elements ((music
							  'SkipEvent
							  duration (ly:make-duration 0 0 0 1))
							 (music
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
					       elements ((music
							  'SkipEvent
							  duration (ly:make-duration 0 0 0 1))
							 (music
							  'SlurEvent
							  span-direction START)))
					      (music
					       'ContextSpeccedMusic
					       element (music
							'OverrideProperty
							grob-property-path '(stroke-style)
							grob-value "grace"
							symbol 'Stem)))))
			   #t)
	 (with-music-match (?stop (music 
				   'SequentialMusic
				   elements ((music
					      'ContextSpeccedMusic
					      element (music
						       'RevertProperty
						       grob-property-path '(stroke-style)
						       symbol 'Stem))
					     (music
					      'EventChord
					      elements ((music
							 'SkipEvent
							 duration (ly:make-duration 0 0 0 1))
							(music
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
	(cluster? (make-music-type-predicate 'ClusterNoteEvent))
	(note? (make-music-type-predicate 'NoteEvent)))
    (format #f "~a~a{~v%~v_~{~a ~}~v%~v_}"
	    (if (any (lambda (e)
		       (and (chord? e)
			    (any cluster? (ly:music-property e 'elements))))
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
				   (ly:music-property chord 'elements)))
			    (filter chord? elements))
		       "\\lyricmode ")
		      ((any (lambda (chord)
			      (any (lambda (event)
				     (and (note? event)
					  (not (null? (ly:music-property event 'drum-type)))))
				   (ly:music-property chord 'elements)))
			    (filter chord? elements))
		       "\\drummode ")
		      (else ;; TODO: other modes?
		       ""))
		"")
	    (if force-line-break 1 0)
	    (if force-line-break (+ 2 (*indent*)) 1)
	    (parameterize ((*indent* (+ 2 (*indent*))))
			  (map-in-order music->lily-string elements))
	    (if force-line-break 1 0)
	    (if force-line-break (*indent*) 0))))

(define-display-method SimultaneousMusic (sim)
  (parameterize ((*indent* (+ 3 (*indent*))))
    (format #f "<< ~{~a ~}>>"
	    (map-in-order music->lily-string (ly:music-property sim 'elements)))))

(define-extra-display-method SimultaneousMusic (expr)
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
	    (music->lily-string ?before-grace)
	    (music->lily-string ?grace))))
  
;;;
;;; Chords
;;;

(define-display-method EventChord (chord)
  ;; event_chord : simple_element post_events
  ;;		   | command_element
  ;;		   | note_chord_element

  ;; TODO : tagged post_events
  ;; post_events : ( post_event | tagged_post_event )*
  ;; tagged_post_event: '-' \tag embedded_scm post_event

  (let* ((elements (ly:music-property chord 'elements))
	 (simple-elements (filter (make-music-type-predicate 
				   'NoteEvent 'ClusterNoteEvent 'RestEvent
				   'MultiMeasureRestEvent 'SkipEvent 'LyricEvent)
				  elements)))
    (if ((make-music-type-predicate 'StaffSpanEvent 'BreathingSignEvent) (car elements))
	;; first, a special case: StaffSpanEvent (\startStaff, \stopStaff)
	;; and BreathingSignEvent (\breathe)
	(music->lily-string (car elements))
	(if (and (not (null? simple-elements))
		 (null? (cdr simple-elements)))
	    ;; simple_element : note | figure | rest | mmrest | lyric_element | skip
	    (let* ((simple-element (car simple-elements))
		   (duration (ly:music-property simple-element 'duration))
		   (lily-string (format #f "~a~a~a~{~a ~}"
					(music->lily-string simple-element)
					(duration->lily-string duration)
					(if (and ((make-music-type-predicate 'RestEvent) simple-element)
						 (ly:pitch? (ly:music-property simple-element 'pitch)))
					    "\\rest"
					    "")
					(map-in-order music->lily-string (filter post-event? elements)))))
	      (*previous-duration* duration)
	      lily-string)
	    (let ((chord-elements (filter (make-music-type-predicate
					   'NoteEvent 'ClusterNoteEvent 'BassFigureEvent)
					  elements))
		  (post-events (filter post-event? elements)))
	      (if (not (null? chord-elements))
		  ;; note_chord_element : '<' (notepitch | drumpitch)* '>" duration post_events
		  (let ((lily-string (format #f "< ~{~a ~}>~a~{~a ~}"
					     (map-in-order music->lily-string chord-elements)
					     (duration->lily-string (ly:music-property (car chord-elements)
										     'duration))
					     (map-in-order music->lily-string post-events))))
		    (*previous-duration* (ly:music-property (car chord-elements) 'duration))
		    lily-string)
		  ;; command_element
		  (format #f "~{~a ~}" (map-in-order music->lily-string elements))))))))

(define-display-method MultiMeasureRestMusicGroup (mmrest)
  (format #f "~{~a ~}"
	  (map-in-order music->lily-string 
			(remove (make-music-type-predicate 'BarCheck)
				(ly:music-property mmrest 'elements)))))

(define-display-method SkipMusic (skip)
  (format #f "\\skip ~a" (duration->lily-string (ly:music-property skip 'duration) #:force-duration #t)))

;;;
;;; Notes, rests, skips...
;;;

(define (simple-note->lily-string event)
  (format #f "~a~a~a~a~{~a~}" ; pitchname octave !? octave-check articulations
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
	  (map-in-order music->lily-string (ly:music-property event 'articulations))))

(define-display-method NoteEvent (note)
  (cond ((not (null? (ly:music-property note 'pitch))) ;; note
	 (simple-note->lily-string note))
	((not (null? (ly:music-property note 'drum-type))) ;; drum
	 (format #f "~a" (ly:music-property note 'drum-type)))
	(else ;; unknown?
	 "")))

(define-display-method ClusterNoteEvent (note)
  (simple-note->lily-string note))

(define-display-method RestEvent (rest)
  (if (not (null? (ly:music-property rest 'pitch)))
      (simple-note->lily-string rest)
      "r"))

(define-display-method MultiMeasureRestEvent (rest)
  "R")

(define-display-method SkipEvent (rest)
  "s")

(define-display-method MarkEvent (mark)
  (let ((label (ly:music-property mark 'label)))
    (if (null? label)
	"\\mark \\default"
	(format #f "\\mark ~a" (markup->lily-string label)))))

(define-display-method MetronomeChangeEvent (tempo)
  (format #f "\\tempo ~a = ~a"
	  (duration->lily-string (ly:music-property tempo 'tempo-unit) #:force-duration #f #:prev-duration #f)
	  (ly:music-property tempo 'metronome-count)))

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
			 (if (and (*parser*)
				  (equal? (ly:parser-lookup (*parser*) mode) c-pitch-alist))
			     (symbol->string mode)
			     #f))
		       '(major minor ionian locrian aeolian mixolydian lydian phrygian dorian))
		  (new-line->lily-string))))))

(define-display-method RelativeOctaveCheck (octave)
  (let ((pitch (ly:music-property octave 'pitch)))
    (format #f "\\octave ~a~a"
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

(define-display-method BreakEvent (br)
  "\\break") ;; TODO: use page-penalty, penalty properties?

(define-display-method PesOrFlexaEvent (expr)
  "\\~")

(define-display-method BassFigureEvent (figure)
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
		(case alteration
		  ((-4) "--")
		  ((-2) "-")
		  ((0) "!")
		  ((2) "+")
		  ((4) "++")
		  (else "")))
	    (if (null? bracket-stop) "" "]"))))

(define-display-method LyricEvent (lyric)
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
	(markup->lily-string text))))

(define-display-method BreathingSignEvent (event)
  "\\breathe")

;;;
;;; Staff switches
;;;

(define-display-method AutoChangeMusic (m)
  (format #f "\\autochange ~a"
	  (music->lily-string (ly:music-property m 'element))))

(define-display-method ContextChange (m)
  (format #f "\\change ~a = \"~a\""
	  (ly:music-property m 'change-to-type)
	  (ly:music-property m 'change-to-id)))

;;;

(define-display-method TimeScaledMusic (times)
  (let* ((num (ly:music-property times 'numerator))
	 (den (ly:music-property times 'denominator))
	 (nd-gcd (gcd num den)))
    (parameterize ((*force-line-break* #f)
		   (*time-factor-numerator* (/ num nd-gcd))
		   (*time-factor-denominator* (/ den nd-gcd)))
      (format #f "\\times ~a/~a ~a" 
	      num
	      den
	      (music->lily-string (ly:music-property times 'element))))))

(define-display-method RelativeOctaveMusic (m)
  (music->lily-string (ly:music-property m 'element)))

(define-display-method TransposedMusic (m)
  (music->lily-string (ly:music-property m 'element)))

;;;
;;; Repeats
;;;

(define (repeat->lily-string expr repeat-type)
  (format #f "\\repeat ~a ~a ~a ~a"
	  repeat-type
	  (ly:music-property expr 'repeat-count)
	  (music->lily-string (ly:music-property expr 'element))
	  (let ((alternatives (ly:music-property expr 'elements)))
	    (if (null? alternatives)
		""
		(format #f "\\alternative { ~{~a ~}}"
			(map-in-order music->lily-string alternatives))))))

(define-display-method VoltaRepeatedMusic (expr)
  (repeat->lily-string expr "volta"))

(define-display-method UnfoldedRepeatedMusic (expr)
  (repeat->lily-string expr "unfold"))

(define-display-method FoldedRepeatedMusic (expr)
  (repeat->lily-string expr "fold"))

(define-display-method PercentRepeatedMusic (expr)
  (repeat->lily-string expr "percent"))

(define-display-method TremoloRepeatedMusic (expr)
  (let* ((count (ly:music-property expr 'repeat-count))
	 (dots (if (= 0 (modulo count 3)) 0 1))
	 (shift (- (log2 (if (= 0 dots)
			     (/ (* count 2) 3)
			     count))))
	 (element (ly:music-property expr 'element))
	 (den-mult 1))
    (if (eqv? (ly:music-property element 'name) 'SequentialMusic)
	(begin
	  (set! shift (1- shift))
	  (set! den-mult (length (ly:music-property element 'elements)))))
    (music-map (lambda (m)
		 (let ((duration (ly:music-property m 'duration)))
		   (if (ly:duration? duration)
		       (let* ((dlog (ly:duration-log duration))
			      (ddots (ly:duration-dot-count duration))
			      (dfactor (ly:duration-factor duration))
			      (dnum (car dfactor))
			      (dden (cdr dfactor)))
			 (set! (ly:music-property m 'duration)
			       (ly:make-duration (- dlog shift)
						 ddots ;;(- ddots dots) ; ????
						 dnum
						 (/ dden den-mult))))))
		 m)
	       element)
    (format #f "\\repeat tremolo ~a ~a"
	    count
	    (music->lily-string element))))

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
			       (reverse operations)))
			(*indent*)))
	    (parameterize ((*current-context* ctype))
	      (music->lily-string music)))))

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

(define (property-value->lily-string arg)
  (cond ((ly:music? arg)
	 (music->lily-string arg))
	((string? arg)
	 (format #f "#~s" arg))
	((markup? arg)
	 (markup->lily-string arg))
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
	    (if (eqv? (*current-context*) 'Bottom) 
		"" 
		(format #f "~a . " (*current-context*)))
	    property
	    (property-value->lily-string value)
	    (new-line->lily-string))))

(define-display-method PropertyUnset (expr)
  (format #f "\\unset ~a~a~a"
	  (if (eqv? (*current-context*) 'Bottom) 
	      "" 
	      (format #f "~a . " (*current-context*)))
	  (ly:music-property expr 'symbol)
	  (new-line->lily-string)))

;;; Layout properties

(define-display-method OverrideProperty (expr)
  (let ((symbol	  (ly:music-property expr 'symbol))
	(properties (ly:music-property expr 'grob-property-path))
	(value	  (ly:music-property expr 'grob-value))
	(once	  (ly:music-property expr 'once)))
    (format #f "~a\\override ~a~a #'~a = ~a~a"
	    (if (or (null? once)
		    (not once))
		""
		"\\once ")
	    (if (eqv? (*current-context*) 'Bottom) 
		"" 
		(format #f "~a . " (*current-context*)))
	    symbol
	    (if (null? (cdr properties))
		(car properties)
		properties)
	    (property-value->lily-string value)
	    (new-line->lily-string))))
	    
(define-display-method RevertProperty (expr)
  (let ((symbol (ly:music-property expr 'symbol))
	(properties (ly:music-property expr 'grob-property-path)))
    (format #f "\\revert ~a~a #'~a~a"
	    (if (eqv? (*current-context*) 'Bottom) 
		"" 
		(format #f "~a . " (*current-context*)))
	    symbol
	    (if (null? (cdr properties))
		(car properties)
		properties)
	    (new-line->lily-string))))

;;; \clef 
(define clef-name-alist (map (lambda (name+vals)
			       (cons (cdr name+vals)
				     (car name+vals)))
			     supported-clefs))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a clef change, return \"\\clef ...\"
Otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
				 context-type 'Staff
				 element (music 'SequentialMusic
						elements ((music 'PropertySet
								 value ?clef-glyph
								 symbol 'clefGlyph)
							  (music 'PropertySet
								 symbol 'middleCPosition)
							  (music 'PropertySet
								 value ?clef-position
								 symbol 'clefPosition)
							  (music 'PropertySet
								 value ?clef-octavation
								 symbol 'clefOctavation)))))
    (let ((clef-prop+name (assoc (list ?clef-glyph ?clef-position 0)
				 clef-name-alist)))
      (if clef-prop+name
	  (format #f "\\clef \"~a~{~a~a~}\"~a"
		  (cdr clef-prop+name)
		  (cond ((= 0 ?clef-octavation)
			 (list "" ""))
			((> ?clef-octavation 0)
			 (list "^" (1+ ?clef-octavation)))
			(else
			 (list "_" (- 1 ?clef-octavation))))
		  (new-line->lily-string))
	  #f))))

;;; \time
(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a time signature set, return \"\\time ...\".
Otherwise, return #f."
  (with-music-match (expr (music 
			   'ContextSpeccedMusic
			   element (music 
				    'ContextSpeccedMusic
				    context-type 'Timing
				    element (music 
					     'SequentialMusic
					     elements ((music 
							'PropertySet
							value ?num+den
							symbol 'timeSignatureFraction)
						       (music
							'PropertySet
							symbol 'beatLength)
						       (music
							'PropertySet
							symbol 'measureLength)
						       (music
							'PropertySet
							value ?grouping
							symbol 'beatGrouping))))))
    (if (null? ?grouping)
	(format #f "\\time ~a/~a~a" (car ?num+den) (cdr ?num+den) (new-line->lily-string))
	(format #f "#(set-time-signature ~a ~a '~s)~a"
		(car ?num+den) (cdr ?num+den) ?grouping (new-line->lily-string)))))

;;; \bar
(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a bar, return \"\\bar ...\".
Otherwise, return #f."
  (with-music-match (expr (music 'ContextSpeccedMusic
                                 context-type 'Timing
                                 element (music 'PropertySet
                                                value ?bar-type
                                                symbol 'whichBar)))
     (format #f "\\bar \"~a\"~a" ?bar-type (new-line->lily-string))))

;;; \partial
(define (duration->moment ly-duration)
  (let ((log2	 (ly:duration-log ly-duration))
	(dots	 (ly:duration-dot-count ly-duration))
	(num+den (ly:duration-factor ly-duration)))
    (let* ((m (expt 2 (- log2)))
	   (factor (/ (car num+den) (cdr num+den))))
      (/ (do ((i 0 (1+ i))
	      (delta (/ m 2) (/ delta 2)))
	     ((= i dots) m)
	   (set! m (+ m delta)))
	 factor))))
(define moment-duration-alist (map (lambda (duration)
				     (cons (duration->moment duration)
					   duration))
				   (append-map (lambda (log2)
						 (map (lambda (dots)
							(ly:make-duration log2 dots 1 1))
						      (list 0 1 2 3)))
					       (list 0 1 2 3 4))))

(define (moment->duration moment)
  (let ((result (assoc (- moment) moment-duration-alist)))
    (and result 
	 (cdr result))))

(define-extra-display-method ContextSpeccedMusic (expr)
  "If `expr' is a partial measure, return \"\\partial ...\".
Otherwise, return #f."
  (with-music-match (expr (music
			   'ContextSpeccedMusic
			   element (music
				    'ContextSpeccedMusic
				    context-type 'Timing
				    element (music
					     'PropertySet
					     value ?moment
					     symbol 'measurePosition))))
     (let ((duration (moment->duration (/ (ly:moment-main-numerator ?moment)
					  (ly:moment-main-denominator ?moment)))))
       (and duration (format #f "\\partial ~a" (duration->lily-string duration #:force-duration #t))))))

;;;
;;;

(define-display-method ApplyOutputEvent (applyoutput)
  (let ((proc (ly:music-property applyoutput 'procedure)))
    (format #f "\\applyOutput #~a"
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

;;; \partcombine
(define-display-method PartCombineMusic (expr)
  (format #f "\\partcombine ~{~a ~}"
	  (map-in-order music->lily-string (ly:music-property expr 'elements))))

(define-extra-display-method PartCombineMusic (expr)
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
	    (music->lily-string ?sequence1)
	    (new-line->lily-string)
	    (music->lily-string ?sequence2))))

(define-display-method UnrelativableMusic (expr)
  (music->lily-string (ly:music-property expr 'element)))

;;; Cue notes
(define-display-method QuoteMusic (expr)
  (or (with-music-match (expr (music
			       'QuoteMusic
			       quoted-voice-direction ?quoted-voice-direction
			       quoted-music-name ?quoted-music-name
			       quoted-context-id "cue"
			       quoted-context-type 'Voice
			       element ?music))
	(format #f "\\cueDuring #~s #~a ~a"
		?quoted-music-name
		?quoted-voice-direction
		(music->lily-string ?music)))
      (format #f "\\quoteDuring #~s ~a"
	      (ly:music-property expr 'quoted-music-name)
	      (music->lily-string (ly:music-property expr 'element)))))

;;;
;;; Lyrics
;;;

;;; \lyricsto
(define-display-method LyricCombineMusic (expr)
  (format #f "\\lyricsto ~s ~a"
	  (ly:music-property expr 'associated-context)
	  (parameterize ((*explicit-mode* #f))
	    (music->lily-string (ly:music-property expr 'element)))))

(define-display-method OldLyricCombineMusic (expr)
  (format #f "\\oldaddlyrics ~a~a~a"
	  (music->lily-string (first (ly:music-property expr 'elements)))
	  (new-line->lily-string)
	  (music->lily-string (second (ly:music-property expr 'elements)))))

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
		(parameterize ((*explicit-mode* #f))
		  (music->lily-string ?lyric-sequence)))
	#f)))


