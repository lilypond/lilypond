;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2010 Erik Sandberg <mandolaerik@gmail.com>
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

;; TODO: use separate module for syntax
;; constructors. Also create wrapper around the constructor?
(define define-ly-syntax define-public)

;; A ly-syntax constructor takes two extra parameters, parser and
;; location. These are mainly used for reporting errors and
;; warnings. This function is a syntactic sugar which uses the
;; location arg to set the origin of the returned music object; this
;; behaviour is usually desired
(defmacro define-ly-syntax-loc (args . body)
  (primitive-eval `(define-ly-syntax ,args
		     (let ((m ,(cons 'begin body)))
		       (set! (ly:music-property m 'origin) ,(third args))
		       m))))

;; Like define-ly-syntax-loc, but adds parser and location
;; parameters. Useful for simple constructors that don't need to
;; report errors.
(defmacro define-ly-syntax-simple (args . body)
  (primitive-eval `(define-ly-syntax ,(cons* (car args) 
					     'parser
					     'location 
					     (cdr args))
		     (let ((m ,(cons 'begin body)))
		       (set! (ly:music-property m 'origin) location)
		       m))))

;; Music function: Apply function and check return value.
(define-ly-syntax-loc (music-function parser loc fun args)
  (let ((m (apply fun (cons* parser loc args))))
    (if (ly:music? m)
	m
	(begin
	  (ly:parser-error parser (_ "Music head function must return Music object") loc)
	  (make-music 'Music)))))

(define-ly-syntax-simple (void-music)
  (make-music 'Music))

(define-ly-syntax-simple (sequential-music mlist)
  (make-sequential-music mlist))

(define-ly-syntax-simple (simultaneous-music mlist)
  (make-simultaneous-music mlist))

(define-ly-syntax-simple (event-chord mlist)
  (make-music 'EventChord
	      'elements mlist))

(define-ly-syntax-simple (unrelativable-music mus)
  (make-music 'UnrelativableMusic
	      'element mus))

(define-ly-syntax-simple (context-change type id)
  (make-music 'ContextChange
	      'change-to-type type
	      'change-to-id id))

(define-ly-syntax-simple (voice-separator)
  (make-music 'VoiceSeparator))

(define-ly-syntax-simple (bar-check)
  (make-music 'BarCheck))

(define-ly-syntax-simple (time-scaled-music fraction music)
  (make-music 'TimeScaledMusic
  	      'element (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction)))
  	      'numerator (car fraction)
  	      'denominator (cdr fraction)))

(define-ly-syntax-simple (transpose-music pitch music)
  (make-music 'TransposedMusic
  	      'element (ly:music-transpose music pitch)))

(define-ly-syntax-simple (tempo text duration tempo)
  (let ((props (list
		  (make-property-set 'tempoWholesPerMinute
			(ly:moment-mul (ly:make-moment tempo 1)
				       (ly:duration-length duration)))
		  (make-property-set 'tempoUnitDuration duration)
		  (make-property-set 'tempoUnitCount tempo))))
    (set! props (cons 
            (if text (make-property-set 'tempoText text)
                     (make-property-unset 'tempoText)) 
            props))
    (context-spec-music
      (make-sequential-music props)
      'Score)))

(define-ly-syntax-simple (tempoText text)
  (context-spec-music
   (make-sequential-music
    (list
     (make-property-unset 'tempoUnitDuration)
     (make-property-unset 'tempoUnitCount)
     (make-property-set 'tempoText text)))
   'Score))

(define-ly-syntax-simple (skip-music dur)
  (make-music 'SkipMusic
	      'duration dur))

(define-ly-syntax-simple (repeat type num body alts)
  (make-repeat type num body alts))

(define (script-to-mmrest-text music)
  "Extract 'direction and 'text from SCRIPT-MUSIC, and transform MultiMeasureTextEvent"

  (if (memq 'script-event (ly:music-property music 'types))
      
      (let*
	  ((dir (ly:music-property music 'direction))
	   (tags (ly:music-property music 'tags))
	   (p   (make-music 'MultiMeasureTextEvent
			     'tags tags
			     'text (ly:music-property music 'text))))
	(if (ly:dir? dir)
	    (set! (ly:music-property p 'direction) dir))
	p)
      music))

(define-ly-syntax (multi-measure-rest parser location duration articulations)
  (make-music 'MultiMeasureRestMusic
	      'articulations (map script-to-mmrest-text articulations)
	      'duration duration
	      'origin location))

(define-ly-syntax (repetition-chord parser location previous-chord repetition-function duration articulations)
  (make-music 'RepeatedChord
	      'original-chord previous-chord
	      'element (repetition-function previous-chord location duration articulations)
	      'origin location))

(define-ly-syntax-simple (context-specification type id mus ops create-new)
  (let* ((type-sym (if (symbol? type) type (string->symbol type)))
	 (csm (context-spec-music mus type-sym id)))
    (set! (ly:music-property csm 'property-operations) ops)
    (if create-new (set! (ly:music-property csm 'create-new) #t))
    csm))

(define-ly-syntax (property-operation parser location once ctx music-type symbol . args)
  (let* ((props (case music-type
		  ((PropertySet) (list 'value (car args)))
		  ((PropertyUnset) '())
		  ((OverrideProperty) (list 'grob-value (car args)
					    'grob-property-path (if (list? (cadr args))
								    (cadr args)
								    (cdr args))
					    'pop-first #t))
		  ((RevertProperty)
		   (if (list? (car args))
		       (list 'grob-property-path (car args))
		       (list 'grob-property-path args)))
		  (else (ly:error (_ "Invalid property operation ~a") music-type))))
	 (oprops (if once (cons* 'once once props) props))
	 (m (apply make-music music-type
		   'symbol symbol
		   'origin location
		   oprops)))
    (make-music 'ContextSpeccedMusic
		'element m
		'context-type ctx
		'origin location)))

;; TODO: It seems that this function rarely returns anything useful.
(define (get-first-context-id type mus)
  "Find the name of a ContextSpeccedMusic with given type"
  (let ((id (ly:music-property mus 'context-id)))
    (if (and (eq? (ly:music-property mus 'type) 'ContextSpeccedMusic)
	     (eq? (ly:music-property mus 'context-type) type)
	     (string? id)
	     (not (string-null? id)))
	id
	'())))

(define unique-counter -1)
(define (get-next-unique-voice-name)
  (set! unique-counter (1+ unique-counter))
  (call-with-output-string (lambda (p) (format p "uniqueContext~s" unique-counter))))

(define (lyric-combine-music sync music loc)
  (make-music 'LyricCombineMusic
	      'element music
	      'associated-context sync
	      'origin loc))

(define-ly-syntax (lyric-combine parser location voice music)
  (lyric-combine-music voice music location))

(define-ly-syntax (add-lyrics parser location music addlyrics-list)
  (let* ((existing-voice-name (get-first-context-id 'Voice music))
	 (voice-name (if (string? existing-voice-name)
			 existing-voice-name
			 (get-next-unique-voice-name)))
	 (voice (if (string? existing-voice-name)
		    (music)
		    (make-music 'ContextSpeccedMusic
				'element music
				'context-type 'Voice
				'context-id voice-name
				'origin (ly:music-property music 'origin))))
	 (lyricstos (map (lambda (mus)
			   (let* ((loc (ly:music-property mus 'origin))
				  (lyr (lyric-combine-music voice-name mus loc)))
			     (make-music 'ContextSpeccedMusic
					 'create-new #t
					 'context-type 'Lyrics
					 'element lyr
					 'origin loc)))
			 addlyrics-list)))
    (make-simultaneous-music (cons voice lyricstos))))
