;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Mark Polesky <markpolesky@yahoo.com>
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

(define (document-type-predicate entry)
  (let ((pred (procedure-name (car entry))))
    (string-append
     "@item @code{"
     (case pred
       ;; don't print "cheap-markup?"
       ((cheap-markup?) "markup?")
       (else (symbol->string pred)))
     "} @tab \n"
     (case pred
       ;; clarify `list?' vs. `cheap-list?'
       ((list?) "list @emph{(use} @code{cheap-list?}
                @emph{for faster processing)}")
       ((cheap-list?) "list @emph{(use this instead of}
                      @code{list?} @emph{for faster processing)}")
       (else (cdr entry)))
     "\n")))

(define (document-type-predicate-category alist nodename description)
  (string-append
   "@node " nodename "\n"
   "@unnumberedsubsec " nodename "\n"
   "\n\n"
   description "\n\n"
   "@multitable @columnfractions .33 .66\n"
   "@headitem Type predicate @tab Description\n"
   (string-concatenate
    (sort (map document-type-predicate alist)
          ly:string-ci<?))
   "@end multitable\n"
   "\n"))

(define-public type-predicates-doc-string
  (string-append
   "Predicates return @code{#t} when their argument is of the named type
and @code{#f} if it isn't.\n\n"
   "@menu\n"
   "* R5RS primary predicates::\n"
   "* R5RS secondary predicates::\n"
   "* Guile predicates::\n"
   "* LilyPond scheme predicates::\n"
   "* LilyPond exported predicates::\n"
   "@end menu\n"
   "\n"
   (string-concatenate
    (map
     (lambda (alist-nodename-list)
       (apply document-type-predicate-category
              alist-nodename-list))
     `((,r5rs-primary-predicates
        "R5RS primary predicates"
        "Primary predicates can be applied to any expression.  They can
be used on their own as predicates for LilyPond functions.
The predicates here are part of the Scheme standard R5RS.")
       (,r5rs-secondary-predicates
        "R5RS secondary predicates"
        "Secondary predicates are only applicable to specific expressions
(for example, to numbers).  They will throw a type error when applied to expressions
they are not intended for.  The predicates here are part of the Scheme standard R5RS.")
       (,guile-predicates
        "Guile predicates"
        "These predicates are defined by Guile but are not part of a Scheme standard.")
       (,lilypond-scheme-predicates
        "LilyPond scheme predicates"
        "These predicates are only available within LilyPond and defined in Scheme.")
       (,lilypond-exported-predicates
        "LilyPond exported predicates"
        "These predicates are only available within LilyPond and usually defined in C++."))))))
