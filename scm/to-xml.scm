(use-modules (ice-9 regex))
"


Todo: this is a quick hack; it makes more sense to define a GOOPS
class of a documentnode (similar to how
; the documentation is generated.)

That is much cleaner: building the document, and dumping it to output
is then separated.

"


(define (dtd-header)
  (string-append
   "<?xml version=\"1.0\"?>
<!DOCTYPE MUSIC ["
   preliminary-dtd
   "
]>

"))
  
 
;; as computed from input/trip.ly, by
;; http://www.pault.com/pault/dtdgenerator/

;; must recompute with larger, more serious piece, and probably
;; manually add stuff
(define preliminary-dtd
  "
<!ELEMENT duration EMPTY >
<!ATTLIST duration denom ( 1 | 3 | 5 ) #REQUIRED >
<!ATTLIST duration dots ( 0 | 1 ) #REQUIRED >
<!ATTLIST duration log ( 0 | 1 | 2 | 3 | 4 ) #REQUIRED >
<!ATTLIST duration numer ( 1 | 4 ) #REQUIRED >

<!ELEMENT music ( duration | music | pitch )* >
<!ATTLIST music articulation-type ( lheel | ltoe | marcato | rheel | rtoe | staccato | tenuto ) #IMPLIED >
<!ATTLIST music change-to-id NMTOKEN #IMPLIED >
<!ATTLIST music change-to-type NMTOKEN #IMPLIED >
<!ATTLIST music context-id CDATA #IMPLIED >
<!ATTLIST music context-type ( PianoStaff | Score | Staff | Timing | Voice ) #IMPLIED >
<!ATTLIST music denominator NMTOKEN #IMPLIED >
<!ATTLIST music direction ( 0 | 1 ) #IMPLIED >
<!ATTLIST music force-accidental CDATA #IMPLIED >
<!ATTLIST music grob-property NMTOKEN #IMPLIED >
<!ATTLIST music grob-value CDATA #IMPLIED >
<!ATTLIST music iterator-ctor CDATA #IMPLIED >
<!ATTLIST music label NMTOKEN #IMPLIED >
<!ATTLIST music last-pitch CDATA #IMPLIED >
<!ATTLIST music numerator NMTOKEN #IMPLIED >
<!ATTLIST music penalty NMTOKEN #IMPLIED >
<!ATTLIST music pitch-alist CDATA #IMPLIED >
<!ATTLIST music pop-first CDATA #IMPLIED >
<!ATTLIST music repeat-count NMTOKEN #IMPLIED >
<!ATTLIST music span-direction ( -1 | 1 ) #IMPLIED >
<!ATTLIST music span-type NMTOKEN #IMPLIED >
<!ATTLIST music symbol NMTOKEN #IMPLIED >
<!ATTLIST music text NMTOKEN #IMPLIED >
<!ATTLIST music text-type NMTOKEN #IMPLIED >
<!ATTLIST music type NMTOKEN #REQUIRED >
<!ATTLIST music value CDATA #IMPLIED >

<!ELEMENT pitch EMPTY >
<!ATTLIST pitch alteration ( 0 | 1 ) #REQUIRED >
<!ATTLIST pitch notename ( 0 | 1 | 2 | 3 | 4 | 5 | 6 ) #REQUIRED >
<!ATTLIST pitch octave ( -1 | -2 | 0 | 1 ) #REQUIRED >")




(define (dump-duration d port)
 (display (open-tag 'duration
	    `((log . ,(duration-log d))
	      (dots . ,(duration-dot-count d))
	      (numer . ,(car (duration-factor d)))
	      (denom . ,(cdr (duration-factor d)))
	      )
	    '() ) port)
 (display  (close-tag 'duration) port))

(define (dump-pitch p port)
 (display (open-tag 'pitch
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

(define (re-sub re to string)
  (regexp-substitute/global #f re string 'pre to 'post))

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


    (let ((s (call-with-output-string (lambda (port) (display val port)))))
      ;; ugh
      (re-sub
       "\"" "&quot;"
       (re-sub
	"<"  "&lt;"
	(re-sub
	 ">"  "&gt;"
	 (re-sub
	  "'"  "&apos;"
	  (re-sub
	   "&" "&amp;" s))))))
     
    "\""
    )))

  (string-append
   "<" (symbol->string tag)
   (apply string-append
	  (map dump-attr (filter-list candidate? attrs)))

   ">\n")
   
  )
(define (close-tag name)
  (string-append "</" (symbol->string name) ">\n")
  )

(define (music-to-xml-helper music port)
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

     (display (open-tag 'music (cons `(type . ,name) mprops) ignore-props)
	      port)
     (if (duration? d)
	 (dump-duration d port))
     (if (pitch? p)
	 (dump-pitch p port))
     (if (pair? es)
	 (begin
	   (map (lambda (x) (music-to-xml-helper x port)) es)))
     (if (music? e)
	 (begin
	   (music-to-xml-helper e port)))
     (display (close-tag 'music) port)
   ))
   
   
(define-public (music-to-xml music port)
  "Dump XML-ish stuff to PORT."
  (display (dtd-header) port)
  (display (open-tag 'music '((type . score)) '()) port)
  (music-to-xml-helper music port)
  (display (close-tag 'music) port))
