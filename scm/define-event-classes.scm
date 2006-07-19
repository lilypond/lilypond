;;;; stream-event-classes.scm -- define the tree of stream-event classes.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2005-2006 Erik Sandberg <mandolaerik@gmail.com>


(use-modules (srfi srfi-1))

;; Event class hierarchy. Each line is on the form ((List of children) . Parent)
(define event-classes
  '(((StreamEvent) . '())
    ((RemoveContext ChangeParent Override Revert UnsetProperty
      SetProperty MusicEvent OldMusicEvent CreateContext Prepare
      OneTimeStep Finish) . StreamEvent)
    ((arpeggio-event 
      beam-event note-event absolute-dynamic-event
      key-change-event lyric-event pedal-event slur-event tie-event
      metronome-change-event span-dynamic-event)
     . MusicEvent)
    ((decrescendo-event crescendo-event) . span-dynamic-event)
    ((sostenuto-event sustain-event una-corda-event) . pedal-event)
    ((Announcement) . '())
    ((AnnounceNewContext) . Announcement)
    ))

;; Maps event-class to a list of ancestors (inclusive)
;; TODO: use resizable hash
(define ancestor-lookup (make-hash-table 1))

;; Each class will be defined as
;; (class parent grandparent .. )
;; so that (eq? (cdr class) parent) holds.
(for-each
 (lambda (rel)
   (for-each
    (lambda (type)
      (hashq-set! ancestor-lookup type (cons type (hashq-ref ancestor-lookup (cdr rel) '())))) ;; `(define ,type (cons ',type ,(cdr rel)))))
    (car rel)))
 event-classes)

;; TODO: Allow entering more complex classes, by taking unions.
(define-public (ly:make-event-class leaf)
 (hashq-ref ancestor-lookup leaf))
;; (primitive-eval leaf))

(defmacro-public make-stream-event (expr)
  (Stream_event::undump (primitive-eval (list 'quasiquote expr))))

(define* (simplify e)
  (cond
   ;; Special case for lists reduces stack consumption.
   ((list? e) (map simplify e))
   ((pair? e) (cons (simplify (car e))
		    (simplify (cdr e))))
   ((ly:stream-event? e)
    (list 'unquote `(make-stream-event ,(simplify (Stream_event::dump e)))))
   ((ly:music? e)
    (list 'unquote (music->make-music e)))
   ((ly:moment? e)
    (list 'unquote `(ly:make-moment
		     ,(ly:moment-main-numerator e)
		     ,(ly:moment-main-denominator e)
		     . ,(if (eq? 0 (ly:moment-grace-numerator e))
			    '()
			    (list (ly:moment-grace-numerator e)
				  (ly:moment-grace-denominator e))))))
   ((ly:duration? e)
    (list 'unquote `(ly:make-duration
		     ,(ly:duration-log e)
		     ,(ly:duration-dot-count e)
		     ,(car (ly:duration-factor e))
		     ,(cdr (ly:duration-factor e)))))
   ((ly:pitch? e)
    (list 'unquote `(ly:make-pitch
		     ,(ly:pitch-octave e)
		     ,(ly:pitch-notename e)
		     ,(ly:pitch-alteration e))))
   ((ly:input-location? e)
    (list 'unquote '(ly:dummy-input-location)))
   (#t e)))

(define-public (ly:simplify-scheme e)
  (list 'quasiquote (simplify e))
)

; used by lily/dispatcher.cc
(define-public (car< a b) (< (car a) (car b)))
