;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (lily to-xml))

(use-modules (ice-9 regex)
             (srfi srfi-1)
             (lily)
             (oop goops))

"
Todo: this is a quick hack; it makes more sense to define a GOOPS
class of a documentnode (similar to how
the documentation is generated.)

That is much cleaner: building the document, and dumping it to output
is then separated.


   foo = \\score { ... }

   #(as-xml foo)

   <score>
     <music></music>
     <layoutoutput>
     </layoutoutput>
   </score>
"

(define-class <xml-node> ()
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (value #:init-value "" #:accessor node-value #:init-keyword #:value)
  (attributes #:init-value '()
              #:accessor node-attributes
              #:init-keyword #:attributes)
  (children #:init-value '()
            #:accessor node-children
            #:init-keyword #:children))

(define node-names
  '((NoteEvent . note)
    (SequentialMusic . measure)

    ;;ugh
    (pitch . pitch)
    (duration . duration)
    (octave . octave)
    (step . step)))

(define (musicxml-node->string node)
  (let ((xml-name (assoc-get (node-name node) node-names #f)))
    (string-append
     (if xml-name (open-tag xml-name '() '()) "")
     (if (equal? (node-value node) "")
         (string-append
          (if xml-name "\n" "")
          (string-concatenate (map musicxml-node->string (node-children node))))
         (node-value node))
     (if xml-name (close-tag xml-name) "")
     (if xml-name "\n" ""))))

(define (xml-node->string node)
  (string-append
   "\n"
   (open-tag (node-name node) (node-attributes node) '())
   (if (equal? (node-value node) "")
       (string-concatenate (map xml-node->string (node-children node)))
       (node-value node))
   "\n"
   (close-tag (node-name node))))

(define (musicxml-duration->xml-node d)
  (make <xml-node>
    #:name 'duration
    #:value (number->string (ash 1 (ly:duration-log d)))))

(define (duration->xml-node d)
  (make <xml-node>
    #:name 'duration
    ;; #:value (number->string (ash 1 (ly:duration-log d)))))
    #:attributes `((log . ,(ly:duration-log d))
                   (dots . ,(ly:duration-dot-count d))
                   (numer . ,(car (ly:duration-factor d)))
                   (denom . ,(cdr (ly:duration-factor d))))))

(define (pitch->xml-node p)
  (make <xml-node>
    #:name 'pitch
    #:attributes `((octave . ,(ly:pitch-octave p))
                   (notename . ,(ly:pitch-notename p))
                   (alteration . ,(ly:pitch-alteration p)))))

(define (music->xml-node music)
  (let* ((name (ly:music-property music 'name))
         (e (ly:music-property music 'element))
         (es (ly:music-property music 'elements))
         (mprops (ly:music-mutable-properties music))
         (d (ly:music-property music 'duration))
         (p (ly:music-property music 'pitch))
         (ignore-props '(origin elements duration pitch element)))

    (make <xml-node>
      #:name name
      #:children
      (apply
       append
       (if (ly:pitch? p) (list (pitch->xml-node p)) '())
       (if (ly:duration? d) (list (duration->xml-node d)) '())
       (if (pair? es) (map music->xml-node es) '())
       (if (ly:music? e) (list (music->xml-node e)) '())
       '()))))

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


;; should use macro
(define (assert x)
  (if x
      #t
      (ly:error (G_ "assertion failed: ~S") x)))

(define (re-sub re to string)
  (regexp-substitute/global #f re string 'pre to 'post))

(define (re-sub-alist string alist)
  (if (null? alist)
      string
      (re-sub (caar alist) (cdar alist)
              (re-sub-alist string (cdr alist)))))

(define xml-entities-alist
  '(("\"" . "&quot;")
    ("<" . "&lt;")
    (">" . "&gt;")
    ("'" . "&apos;")
    ("&" . "&amp;")))

(define (open-tag tag attrs exceptions)
  (define (candidate? x)
    (not (memq (car x) exceptions)))

  (define (dump-attr sym-val)
    (let* ((sym (car sym-val))
           (val (cdr sym-val)))

      (string-append
       "\n   "
       (symbol->string sym)
       "=\""
       (let ((s (call-with-output-string (lambda (port) (display val port)))))
         (re-sub-alist s xml-entities-alist))
       "\"")))

  (string-append
   "<" (symbol->string tag)
   (string-concatenate (map dump-attr (filter candidate? attrs)))
   ">"))

(define (close-tag name)
  (string-append "</" (symbol->string name) ">"))

(define-public (music-to-xml music port)
  "Dump XML-ish stuff to @var{port}."

  ;; dtd contains # -- This confuses tex during make doc.
  ;;
  ;;  (display (dtd-header) port)

  (display (open-tag 'music '((type . score)) '()) port)
  (display (xml-node->string (music->xml-node music)) port)
  (display (close-tag 'music) port))

(define-public (music-to-musicxml music port)
  "Dump MusicXML-ish stuff to @var{port}."

  ;; dtd contains # -- This confuses tex during make doc.
  ;;
  ;;  (display (dtd-header) port)

  (define duration->xml-node musicxml-duration->xml-node)

  (display (open-tag 'music '((type . score)) '()) port)
  (display (musicxml-node->string (music->xml-node music)) port)
  (display (close-tag 'music) port))
