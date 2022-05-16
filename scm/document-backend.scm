;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;; Jan Nieuwenhuizen <janneke@gnu.org>
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

(define (sort-grob-properties props)
  ;; force 'meta to the end of each prop-list
  (let ((meta (assoc 'meta props)))
    (append (sort (assoc-remove! props 'meta) ly:alist-ci<?)
            (list meta))))

;; properly sort all properties and interfaces
;; within the all-grob-descriptions alist
(set! all-grob-descriptions
      (map!
       (lambda (grob-description)
         (let* ((grob-key      (car grob-description))
                (props         (assoc-ref all-grob-descriptions grob-key))
                (meta          (assoc-ref props 'meta))
                (interfaces    (assoc-ref meta 'interfaces))
                (sorted-ifaces (sort interfaces ly:symbol-ci<?))
                (new-meta      (assoc-set! meta 'interfaces sorted-ifaces))
                (new-props     (assoc-set! props 'meta new-meta))
                (sorted-props  (sort-grob-properties new-props)))
           (cons grob-key sorted-props)))
       all-grob-descriptions))

;; sort all grobs in the all-grob-descriptions alist
(set! all-grob-descriptions (sort all-grob-descriptions ly:alist-ci<?))

(define (interface-doc-string interface)
  (let* ((name (car interface))
         (desc (string-trim-both (cadr interface)))
         (props (caddr interface))
         (docfunc (lambda (pr)
                    (property->texi 'backend pr)))
         (iprops (filter (lambda (x) (object-property x 'backend-internal))
                         props))
         (uprops (filter
                  (lambda (x) (not (object-property x 'backend-internal)))
                  props))
         (user-propdocs (map docfunc uprops))
         (internal-propdocs (map docfunc iprops)))

    (string-append
     desc

     (if (pair? uprops)
         (string-append
          "\n\n@subsubheading User settable properties:\n"
          (description-list->texi user-propdocs #t))
         "")

     (if (pair? iprops)
         (string-append
          "\n\n@subsubheading Internal properties:\n"
          (description-list->texi internal-propdocs #t))
         ""))))

;; The defining interface is expected to be first.
(define class-specific-interfaces
  '((Item . (item-interface))
    (Spanner . (spanner-interface))
    (Paper_column . (paper-column-interface item-interface))
    (System . (system-interface spanner-interface))))


;; Map interface to list of grobs that support it.
(define iface->grob-table (make-hash-table 61))
;; item-interface and spanner-interface (maybe others in
;; the future) are supported "conditionally": a Footnote,
;; for example, supports either of them depending on the
;; object it annotates.  We distinguish this situation in
;; the presentation.
(define class-iface->conditional-grob-table (make-hash-table 4))
(for-each
 (lambda (grob-entry)
   (let* ((grob-name (car grob-entry))
          (grob-description (cdr grob-entry))
          (meta (assoc-get 'meta grob-description))
          (ifaces (assoc-get 'interfaces meta))
          (classes (assoc-get 'classes meta))
          (two-or-more (pair? (cdr classes)))
          (class-table (if two-or-more
                           class-iface->conditional-grob-table
                           iface->grob-table)))

     (for-each (lambda (iface)
                 (hashq-set!
                  iface->grob-table iface
                  (cons grob-name
                        (hashq-ref iface->grob-table iface '()))))
               ifaces)
     (for-each (lambda (class)
                 (let ((class-ifaces
                        (assoc-get class class-specific-interfaces)))
                   (for-each
                    (lambda (class-iface)
                      (hashq-set!
                       class-table
                       class-iface
                       (cons grob-name (hashq-ref class-table class-iface '()))))
                    class-ifaces)))
               classes)))
 all-grob-descriptions)

;; First level Interface description
(define (interface-doc interface)
  (let* ((name (car interface))
         (unconditional-grobs (hashq-ref iface->grob-table name))
         (conditional-grobs (hashq-ref class-iface->conditional-grob-table name)))
    (make <texi-node>
      #:code-tag #t
      #:name (symbol->string name)
      #:text (string-append
              (interface-doc-string (cdr interface))
              "\n\n@raggedRight\n"
              "This grob interface "
              (if unconditional-grobs
                  (format #f
                          "is used in the following graphical object(s): ~a."
                          (list-xref-symbols unconditional-grobs))
                  "is not used in any graphical object.")
              "\n\n"
              (if conditional-grobs
                  (format #f
                          "In addition, this interface is supported conditionally
by the following objects depending on their class: ~a."
                          (list-xref-symbols  conditional-grobs))
                  "")
              "\n@endRaggedRight"))))

(define (grob-alist->texi alist)
  (let* ((uprops (filter (lambda (x) (not (object-property x 'backend-internal)))
                         (map car alist))))

    (description-list->texi
     (map (lambda (y) (property->texi 'backend y alist))
          uprops)
     #t)))

(define (grob-doc description)
  "Given a property alist DESCRIPTION, make a documentation
node."

  (let* ((meta (assoc-get 'meta description))
         (name (assoc-get 'name meta))
         (docstring (assoc-get 'description meta ""))
         (ifaces (assoc-get 'interfaces meta))
         (classes (assoc-get 'classes meta))
         (class-interfaces (map (lambda (class)
                                  (assoc-get class class-specific-interfaces))
                                classes))
         (two-or-more (pair? (cdr classes)))
         (engravers (filter
                     (lambda (x) (engraver-makes-grob? name x))
                     all-engravers-list))
         (namestr (symbol->string name))
         (engraver-names (map ly:translator-name engravers)))

    (make <texi-node>
      #:code-tag #t
      #:name namestr
      #:text
      (string-append
       "\n\n"
       docstring
       "\n\n"
       "@raggedRight\n"
       "@code{"
       namestr "} objects "
       (cond
        ((not (null? engraver-names))
         (string-append
          "are created by: "
          (list-xref-symbols engraver-names)))
        ((eq? name 'System)
         "are created internally by the @code{Score_engraver} translator group.")
        (else
         (ly:error "no engraver declares that it creates ~a grobs" name)))

       "."
       "\n@endRaggedRight"

       "\n\nStandard settings:\n"
       (grob-alist->texi description)
       "\n\n@raggedRight\n"
       "This object supports the following interface(s):\n"
       (list-xref-symbols
        (if two-or-more
            ifaces
            (append ifaces (car class-interfaces))))
       ".\n\n"
       (if two-or-more
           (format #f "This object can be of either of the following classes: ~a.
It supports the following interfaces conditionally depending on the class: ~a."
                   (human-listify
                    (map
                     (lambda (class ifaces)
                       (format #f "~a (characterized by ~a)"
                               class
                               (human-listify (map symbol->string ifaces))))
                     classes
                     class-interfaces)
                    #:last-word "or")
                   ;; No sorting here, let's keep in same order as the
                   ;; parenthesized "characterized by" indications.
                   (list-xref-symbols
                    (apply append class-interfaces)
                    #:sorted #f))
           (format #f "This object is of class ~a (characterized by ~a)."
                   (car classes)
                   (ref-ify (symbol->string (caar class-interfaces)))))
       "\n\n@endRaggedRight"))))

(define (all-grobs-doc)
  (make <texi-node>
    #:name "All layout objects"
    #:desc "Description and defaults for all graphical objects (grobs)."
    #:children
    (map (lambda (x) (grob-doc (cdr x)))  all-grob-descriptions)))

(define interface-description-alist
  (hash-fold
   (lambda (key val prior)
     (cons (cons key val)  prior))
   '() (ly:all-grob-interfaces)))

;; sort user-settable and internal props within each grob-interface
(set! interface-description-alist
      (map! (lambda (iface-desc)
              (let* ((key-name-docstr (list-head iface-desc 3))
                     (props           (list-tail iface-desc 3))
                     (sorted-props    (list (sort (car props) ly:symbol-ci<?))))
                (append key-name-docstr sorted-props)))
            interface-description-alist))

;; sort list of grob interfaces
(set! interface-description-alist
      (sort interface-description-alist ly:alist-ci<?))

;;;;;;;;;; check for dangling backend properties.
(define (mark-interface-properties entry)
  (for-each (lambda (x) (set-object-property! x 'iface-marked #t))
            (caddr (cdr entry))))

(for-each mark-interface-properties interface-description-alist)

(define (check-dangling-properties prop)
  (if (not (object-property prop 'iface-marked))
      (ly:error (string-append "define-grob-properties.scm: "
                               (G_ "cannot find interface for property: ~S")) prop)))

(for-each check-dangling-properties all-backend-properties)

;;;;;;;;;;;;;;;;

(define (lookup-interface name)
  (let* ((entry (hashq-ref (ly:all-grob-interfaces) name #f)))
    (if entry
        entry
        (ly:error (G_ "unknown Grob interface: ~S") name))))

(define (all-interfaces-doc)
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects."
    #:children
    (map interface-doc interface-description-alist)))

(define (backend-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
         (descs (map (lambda (prop)
                       (property->texi 'backend (string->symbol prop) '())) ps))
         (texi (description-list->texi descs #f)))
    texi))

;;(dump-node (grob-doc (cdadr all-grob-descriptions)) (current-output-port) 0 )
(define (backend-doc-node)
  (make <texi-node>
    #:name "Backend"
    #:desc "Reference for the layout engine."
    #:children
    (list
     (all-grobs-doc)
     (all-interfaces-doc)
     (make <texi-node>
       #:name "User backend properties"
       #:desc "All tunable properties in a big list."
       #:text (backend-properties-doc-string all-user-grob-properties))
     (make <texi-node>
       #:name "Internal backend properties"
       #:desc "All internal layout properties in a big list."
       #:text (backend-properties-doc-string all-internal-grob-properties)))))
