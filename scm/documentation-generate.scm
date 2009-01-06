;;;; generate-documentation.scm -- Generate documentation
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
    (dump-node (markup-doc-node) port 2)))

(call-with-output-file "markup-list-commands.tely"
  (lambda (port)
    (dump-node (markup-list-doc-node) port 2)))

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

@include macros.itexi

@ignore
@omftitle LilyPond internals
@omfcreator Han-Wen Nienhuys and Jan Nieuwenhuizen
@omfdescription Programmer's reference of the LilyPond music engraving system
@omftype user's guide
@omflanguage English
@omfcategory Applications|Publishing
@end ignore

@iftex
@afourpaper
@c don't replace quotes with directed quotes
@tex
\\gdef\\SETtxicodequoteundirected{Foo}
\\gdef\\SETtxicodequotebacktick{Bla}
@end tex
@end iftex

@finalout

@titlepage
@title LilyPond
@subtitle The music typesetter
@titlefont{Internals Reference}
@author The LilyPond development team

Copyright @copyright{} 1999--2009 by the authors

@vskip 20pt

For LilyPond version @version{}
@end titlepage

@contents

@ifnottex")
 out-port)

(define top-node
  (make <texi-node>
    #:name "GNU LilyPond -- Internals Reference"
    #:text
    (string-append  "@end ifnottex

@ifhtml
@ifclear bigpage
This document is also available as a
@uref{source/Documentation/user/lilypond-internals.pdf,PDF} and as
@uref{source/Documentation/user/lilypond-internals-big-page.html,one big page}.
@end ifclear
@ifset bigpage
This document is also available as a
@uref{source/Documentation/user/lilypond-internals.pdf,PDF} and as a
@uref{source/Documentation/user/lilypond-internals/index.html,HTML indexed multiple pages}.
@end ifset
@end ifhtml

This is the Internals Reference (IR) for version "
		    (lilypond-version)
		    " of LilyPond, the GNU music typesetter.")

    #:children
    (list
     (music-doc-node)
     (translation-doc-node)
     (backend-doc-node)
     (all-scheme-functions-doc)
     (make <texi-node>
       #:appendix #t
       #:name "Indices"
       #:text "
@appendixsec Concept index

@printindex cp

@appendixsec Function index

@printindex fn

\n@bye"))))

(dump-node top-node out-port 0)
(newline (current-error-port))
