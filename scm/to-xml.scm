

"


Todo: this is a quick hack; it makes more sense to define a GOOPS
class of a documentnode (similar to how
; the documentation is generated.)

That is much cleaner: building the document, and dumping it to output
is then separated.

"

(define (dump-duration d port)
 (display (open-tag "duration"
	    `((log . ,(duration-log d))
	      (dots . ,(duration-dot-count d))
	      (numer . ,(car (duration-factor d)))
	      (denom . ,(cdr (duration-factor d)))
	      )
	    '() ) port)
 (display  (close-tag 'duration) port))

(define (dump-pitch p port)
 (display (open-tag "pitch"
	    `((octave . ,(pitch-octave p))
	      (notename . ,(pitch-notename p))
	      (alteration . ,(pitch-alteration p))
	      )
	    '() ) port)
 (display  (close-tag 'pitch) port))

;; should use macro
(define (assert x)
  (if x
      #t
      (error "assertion failed")))

(define (open-tag tag attrs exceptions)
  (define (candidate? x)
    (not (memq (car x) exceptions)))
  
  (define (dump-attr sym-val)
    (let*
	(
	(sym (car sym-val))
	(val (cdr sym-val))
	)
      
    (string-append
     "\n   "
    (symbol->string sym)
    "=\""
    
    (call-with-output-string (lambda (port) (display val port)))
    "\""
    )))

  (string-append
   "<" tag
   (apply string-append
	  (map dump-attr (filter-list candidate? attrs)))

   ">\n")
   
  )
(define (close-tag name)
  (string-append "</" (symbol->string name) ">\n")
  )

(define-public (music-to-xml music port)
  "Dump XML-ish stuff to PORT."
   (let*
       (
	(name (ly-get-mus-property music 'name))
	(e (ly-get-mus-property music 'element))
	(es (ly-get-mus-property music 'elements))
	(mprops (ly-get-mutable-properties music))
	(p (ly-get-mus-property music 'pitch))
	(d (ly-get-mus-property music 'duration))
	(ignore-props '(origin elements duration pitch element))
	)

     (display (open-tag (symbol->string name) mprops ignore-props) port)
     (if (duration? d)
	 (dump-duration d port))
     (if (pitch? p)
	 (dump-pitch p port))
     (if (pair? es)
	 (begin
	   (display "<elements>" port)
	   (map (lambda (x) (music-to-xml x port)) es)
	   (display "</elements>" port)
	   ))

     (if (music? e)
	 (begin
	   (display "<element>" port)
	   (music-to-xml e port)
	   (display "</element>" port)
	   ))
     (display (close-tag name) port)
   ))
   
   
