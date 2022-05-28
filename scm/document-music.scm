;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define (music-props-doc)
  (make <texi-node>
    #:name "Music properties"
    #:desc "All music properties, including descriptions."
    #:text
    (let* ((ps (sort (map symbol->string all-music-properties) ly:string-ci<?))
           (descs (map (lambda (prop)
                         (property->texi 'music (string->symbol prop)))
                       ps))
           (texi (description-list->texi descs #f)))
      texi)))

(define music-types->names (make-hash-table 61))

(for-each (lambda (entry)
            (let* ((class (ly:camel-case->lisp-identifier (car entry)))
                   (classes (ly:make-event-class class)))
              (if classes
                  (for-each
                   (lambda (cl)
                     (hashq-set! music-types->names cl
                                 (cons (car entry)
                                       (hashq-ref music-types->names cl '()))))
                   classes))))
          music-descriptions)

(define (strip-description x)
  (cons (symbol->string (car x))
        ""))

(define (music-type-doc entry)
  (let* ((accept-list (list-xref-symbols
                       (map ly:translator-name
                            (filter
                             (lambda (x)
                               (engraver-accepts-music-type? (car entry) x))
                             all-engravers-list)))))
    (make <texi-node>
      #:code-tag #t
      #:name (symbol->string (car entry))
      #:text
      (string-append
       "\n@raggedRight"
       "\nMusic event type @code{"
       (symbol->string (car entry))
       "} is in music objects of type "
       (list-xref-symbols (cdr entry))
       "."

       "\n\n"
       (if (equal? accept-list "none")
           "Not accepted by any engraver or performer"
           (string-append
            "Accepted by: "
            accept-list))
       "."
       "\n@endRaggedRight"))))

(define (music-types-doc)
  (make <texi-node>
    #:name "Music classes"
    #:children
    (map music-type-doc
         (sort
          (hash-table->alist music-types->names) ly:alist-ci<?))))

(define (music-doc-str obj)
  (let* ((namesym  (car obj))
         (props (cdr obj))
         (class (ly:camel-case->lisp-identifier namesym))
         (classes (ly:make-event-class class))
         (accept-list (if classes
                          (list-xref-symbols
                           (map ly:translator-name
                                (filter
                                 (lambda (x)
                                   (engraver-accepts-music-types? classes x))
                                 all-engravers-list)))
                          ""))
         (event-texi (if classes
                         (string-append
                          "\n\n@raggedRight\n"
                          "Event classes:\n"
                          (list-xref-symbols classes)
                          "."

                          "\n\n"
                          (if (equal? accept-list "none")
                              "Not accepted by any engraver or performer"
                              (string-append
                               "Accepted by: "
                               accept-list))
                          "."
                          "\n@endRaggedRight")
                         "")))

    (string-append
     (object-property namesym 'music-description)
     event-texi
     "\n\nProperties:\n"
     (description-list->texi
      (map
       (lambda (x) (property->texi 'music x props))
       (sort (map car props) ly:symbol-ci<?))
      #t))))

(define (music-object-doc obj)
  (make <texi-node>
    #:code-tag #t
    #:name (symbol->string (car obj))
    #:text (music-doc-str obj)))

(define (music-expressions-doc)
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
    (map music-object-doc music-descriptions)))

(define (music-doc-node)
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the input data structures."
    #:children
    (list
     (music-expressions-doc)
     (music-types-doc)
     (music-props-doc))))
