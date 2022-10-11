;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(use-modules (ice-9 match))

(define (engraver-makes-grob? name-symbol grav)
  (memq name-symbol (assoc-get 'grobs-created (ly:translator-description grav) '())))

(define (engraver-accepts-music-type? name-symbol grav)
  (memq name-symbol (assoc 'events-accepted (ly:translator-description grav))))

(define (engraver-accepts-music-types? types grav)
  (if (null? types)
      #f
      (or
       (engraver-accepts-music-type? (car types) grav)
       (engraver-accepts-music-types? (cdr types) grav))))

(define (engraver-doc-string engraver in-which-contexts)
  (let* ((propsr (assoc-get 'properties-read (ly:translator-description engraver)))
         (propsw (assoc-get 'properties-written (ly:translator-description engraver)))
         (accepted  (assoc-get 'events-accepted (ly:translator-description engraver)))
         (name-sym  (ly:translator-name engraver))
         (name-str (symbol->string name-sym))
         (desc (string-trim-both (assoc-get 'description (ly:translator-description engraver))))
         (grobs (engraver-grobs engraver)))
    (string-append
     desc
     "\n\n"
     "@raggedRight\n"
     (if (pair? accepted)
         (string-append
          "Music types accepted:\n"
          (list-xref-symbols accepted))
         "")
     "\n\n"
     (if (pair? propsr)
         (string-append
          "Properties (read)"
          (description-list->texi
           (map (lambda (x) (property->texi 'translation x '()))
                (sort propsr ly:symbol-ci<?))
           #t))
         "")

     (if (null? propsw)
         ""
         (string-append
          "Properties (write)"
          (description-list->texi
           (map (lambda (x) (property->texi 'translation x '()))
                (sort propsw ly:symbol-ci<?))
           #t)))

     (if  (null? grobs)
          ""
          (string-append
           "\n\nThis engraver creates the following layout object(s):\n"
           (list-xref-symbols grobs)
           "."))

     "\n\n"

     (if in-which-contexts
         (let* ((output-list
                 (append-map
                  (lambda (output output-name)
                    (let* ((output-alist (ly:output-description output))
                           (context-description-alist (map cdr output-alist))
                           (contexts
                            (append-map
                             (lambda (x)
                               (let* ((context (assoc-get 'context-name x))
                                      (group (assq-ref x 'group-type))
                                      (consists (append
                                                 (if group
                                                     (list group)
                                                     '())
                                                 (assoc-get 'consists x))))
                                 (if (member name-sym consists)
                                     (list context)
                                     '())))
                             context-description-alist)))
                      (if (null? contexts)
                          '()
                          (list (cons output-name (list-xref-symbols contexts))))))
                  (list $defaultlayout $defaultmidi)
                  (list "\\layout" "\\midi"))))
           (string-append
            "@code{" name-str "} "
            (if (null? output-list)
                "is not part of any context"
                (string-append
                 "is part of the following context(s) "
                 (string-join
                  (map
                   (lambda (elt)
                     (string-append "in @code{" (car elt) "}: " (cdr elt)))
                   output-list)
                  ";\n")
                 "."))))
         "")

     "\n@endRaggedRight")))

;; First level Engraver description
(define (engraver-doc grav)
  (make <texi-node>
    #:code-tag #t
    #:name (symbol->string (ly:translator-name grav))
    #:text (engraver-doc-string grav #t)))

;; Second level, part of Context description
(define name->engraver-table (make-hash-table 61))
(for-each
 (lambda (x)
   (hash-set! name->engraver-table (ly:translator-name x) x))
 (ly:get-all-translators))

(define (find-engraver-by-name name)
  "NAME is a symbol."
  (hash-ref name->engraver-table name #f))

(define (document-engraver-by-name name)
  "NAME is a symbol."

  (let* ((eg (find-engraver-by-name name)))

    (cons (ref-ify (symbol->string name))
          (engraver-doc-string eg #f))))

(define (document-property-operation op)
  (match op
    (('assign property (? ly:grob-properties?))
     ;; Ignore definitions of grobs in Global.
     "")
    (('assign property value)
     (format #f "@item Set context property @code{~a} to~a"
             property
             (if (pretty-printable? value)
                 (format #f ":~a\n" (scm->texi value))
                 (format #f " ~a.\n" (scm->texi value)))))
    (('push grob value . path)
     (format #f "@item Set grob property @code{~{~a~^.~}} in @iref{~a} to ~a"
             path
             grob
             (if (pretty-printable? value)
                 (format #f ":~a\n" (scm->texi value))
                 (format #f " ~a.\n" (scm->texi value)))))
    (('unset property)
     (format #f "@item Unset context property @code{~a}\n"
             property))
    (('pop grob . path)
     (format #f "@item Revert grob property @code{~{~a~^.~}} in @iref{~a}\n"
             path
             grob))
    (('apply proc)
     (format #f "@item Apply procedure @code{~a}\n"
             (scm->texi proc)))))


(define (context-doc context-desc)
  (let* ((name-sym (assoc-get 'context-name context-desc))
         (name (symbol->string name-sym))
         (aliases (assoc-get 'aliases context-desc))
         (desc (assoc-get 'description context-desc "(not documented"))
         (accepts (assoc-get 'accepts context-desc))
         (consists (assoc-get 'consists context-desc))
         (props (assoc-get 'property-ops context-desc))
         (defaultchild (assoc-get 'default-child context-desc))
         (grobs-created (context-grobs context-desc)))

    (make <texi-node>
      #:code-tag #t
      #:name name
      #:text
      (string-append
       desc
       (if (pair? aliases)
           (string-append
            "\n\nThis context also accepts commands for the following context(s):\n"
            (list-xref-symbols aliases)
            ".")
           "")

       "\n\n@raggedRight"
       "\nThis context creates the following layout object(s):\n"
       (list-xref-symbols grobs-created #:uniq #t)
       "."

       (if (and (pair? props) (not (null? props)))
           (let ((str (string-concatenate
                       (sort (map document-property-operation props)
                             ly:string-ci<?))))
             (if (string-null? str)
                 ""
                 (string-append
                  "\n\nThis context sets the following properties:\n\n"
                  "@itemize @bullet\n"
                  str
                  "@end itemize\n")))
           "")

       "\n@endRaggedRight"

       (if defaultchild
           (format #f "\n\nThis is not a `Bottom' context; search for such a one will commence after creating an implicit context of type @iref{~a}."
                   defaultchild)
           "\n\nThis is a `Bottom' context; no contexts will be created implicitly from it.")

       (if (null? accepts)
           "\n\nThis context cannot contain other contexts."
           (string-append
            "\n\n@raggedRight"
            "\nContext @code{"
            name
            "} can contain\n"
            (list-xref-symbols accepts)
            "."
            "\n@endRaggedRight"))

       (if (null? consists)
           ""
           (string-append
            "\n\nThis context is built from the following engraver(s):"
            (description-list->texi
             (map document-engraver-by-name (sort consists ly:symbol-ci<?))
             #t)))))))

(define (engraver-grobs grav)
  (let* ((eg (if (symbol? grav)
                 (find-engraver-by-name grav)
                 grav)))
    (if (eq? eg #f)
        '()
        (assoc-get 'grobs-created (ly:translator-description eg) '()))))

(define (context-grobs context-desc)
  (let* ((group (assq-ref context-desc 'group-type))
         (consists (append
                    (if group
                        (list group)
                        '())
                    (assoc-get 'consists context-desc)))
         (grobs (append-map engraver-grobs consists)))
    grobs))

(define (all-contexts-doc)
  (let* ((layout-alist
          (sort (ly:output-description $defaultlayout)
                (lambda (x y) (ly:symbol-ci<? (car x) (car y)))))
         (names (sort (map symbol->string (map car layout-alist)) ly:string-ci<?))
         (contexts (map cdr layout-alist)))

    (make <texi-node>
      #:name "Contexts"
      #:desc "Complete descriptions of all contexts."
      #:children
      (map context-doc contexts))))

(define all-engravers-list  (ly:get-all-translators))
(set! all-engravers-list
      (sort all-engravers-list
            (lambda (a b) (ly:string-ci<? (symbol->string (ly:translator-name a))
                                          (symbol->string (ly:translator-name b))))))

(define (all-engravers-doc)
  (make <texi-node>
    #:name "Engravers and Performers"
    #:desc "All separate engravers and performers."
    #:text "See @ruser{Modifying context plug-ins}."
    #:children
    (map engraver-doc all-engravers-list)))

(define (translation-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
         (sortedsyms (map string->symbol ps))
         (propdescs
          (map
           (lambda (x) (property->texi 'translation  x '()))
           sortedsyms))
         (texi (description-list->texi propdescs #f)))
    texi))

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout."
    #:children
    (list
     (all-contexts-doc)
     (all-engravers-doc)
     (make <texi-node>
       #:name "Tunable context properties"
       #:desc "All tunable context properties."
       #:text (translation-properties-doc-string
               all-user-translation-properties))

     (make <texi-node>
       #:name "Internal context properties"
       #:desc "All internal context properties."
       #:text (translation-properties-doc-string
               all-internal-translation-properties)))))
