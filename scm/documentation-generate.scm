;;;; generate-documentation.scm -- Generate documentation
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;; Jan Nieuwenhuizen <janneke@gnu.org>

;;; File entry point for generated documentation
;;; Running LilyPond on this file generates the documentation

;;(set-debug-cell-accesses! 5000)

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; TODO : make modules of these!
;;;;;;;;;;;;;;;;

;; todo: naming: grob vs. layout property

(map ly:load '("documentation-lib.scm"
	       "document-functions.scm"
	       "document-translation.scm"
	       "document-music.scm"
	       "document-identifiers.scm"
	       "document-backend.scm"
	       "document-markup.scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display
 (slot-ref (all-scheme-functions-doc) 'text)
 (open-output-file "scheme-functions.tely"))

;;(display 
;; (markup-doc-string)
;; (open-output-file "markup-commands.tely"))

(call-with-output-file "markup-commands.tely"
  (lambda (port)
    (dump-node (markup-doc-node) port 2 #t)))

(call-with-output-file "markup-list-commands.tely"
  (lambda (port)
    (dump-node (markup-list-doc-node) port 2 #t)))

(display 
 (identifiers-doc-string)
 (open-output-file "identifiers.tely"))


(display
 (backend-properties-doc-string all-user-grob-properties)
 (open-output-file "layout-properties.tely"))

(display
 (translation-properties-doc-string all-user-translation-properties)
 (open-output-file "context-properties.tely"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file-name "lilypond-internals")
(define outname (string-append file-name ".texi"))

(define out-port (open-output-file outname))

(writing-wip outname)

(display
 (string-append
  (texi-file-head "LilyPond program-reference" file-name
		  "(lilypond/lilypond-internals.info)")
  "

@c NOTE: This is documentation-generate.scm, not macros.itexi


@macro q{TEXT}
@quoteleft{}\\TEXT\\@quoteright{}
@end macro

@macro qq{TEXT}
@quotedblleft{}\\TEXT\\@quotedblright{}
@end macro


@ifhtml
@c ***** HTML *****

@ifset bigpage

@macro ruser{TEXT}
@ref{\\TEXT\\,,,lilypond-big-page,Notation Reference}
@cindex \\TEXT\\
@end macro

@end ifset

@ifclear bigpage

@macro ruser{NAME}
@ref{\\NAME\\,,,lilypond,Notation Reference}
@cindex \\NAME\\
@end macro

@end ifclear

@macro inputfileref{DIR,NAME}
@uref{source/\\DIR\\/out-www/collated-files.html#\\NAME\\,@file{\\DIR\\/\\NAME\\}}@c
@end macro

@end ifhtml


@ifinfo
@c ***** info *****

@macro ruser{NAME}
@ref{\\NAME\\,,,lilypond,Notation Reference}
@cindex \\NAME\\
@end macro

@macro inputfileref{DIR,NAME}
@file{\\DIR\\/\\NAME\\}
@end macro

@end ifinfo


@iftex
@c ***** TeX *****

@macro ruser{NAME}
@ref{\\NAME\\}@c
@end macro

@macro inputfileref{DIR,NAME}@c
@file{\\DIR\\/\\NAME\\}@c
@end macro

@end iftex


@macro rinternals{NAME}
@ref{\\NAME\\}
@end macro


@ignore
@omftitle LilyPond internals
@omfcreator Han-Wen Nienhuys and Jan Nieuwenhuizen
@omfdescription Programmer's reference of the LilyPond music engraving system
@omftype user's guide
@omflanguage English
@omfcategory Applications|Publishing
@end ignore


")
 out-port)

(define top-node
  (make <texi-node>
    #:name "Top"
    #:text 
    (string-append  "This is the program reference for version "
		    (lilypond-version)
		    " of LilyPond, the GNU music typesetter.")

    #:children
    (list
     (music-doc-node)
     (translation-doc-node)
     (backend-doc-node)
     (all-scheme-functions-doc)
     (make <texi-node>
       #:name "Indexes"
       #:text "
@unnumberedsec Concept index

@printindex cp

@unnumberedsec Function index

@printindex fn

\n@bye"))))

(dump-node top-node out-port 0)
(newline (current-error-port))
