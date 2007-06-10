;;;; generate-documentation.scm -- Generate documentation
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(display 
 (markup-doc-string)
 (open-output-file "markup-commands.tely"))

(display 
 (markup-list-doc-string)
 (open-output-file "markup-list-commands.tely"))

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


@ifhtml
@c ***** HTML *****

@macro usermanref{NAME}
@ref{\\NAME\\,,,lilypond}
@cindex \\NAME\\
@end macro

@macro glossaryref{NAME}
@ref{\\NAME\\,,,music-glossary}
@cindex \\NAME\\
@end macro

@macro inputfileref{DIR,NAME}
@uref{source/\\DIR\\/out-www/collated-files.html#\\NAME\\,@file{\\DIR\\/\\NAME\\}}@c
@end macro

@macro q{TEXT}
@html
&lsquo;\\TEXT\\&rsquo;
@end html
@end macro
@end ifhtml


@ifinfo
@c ***** info *****

@macro usermanref{NAME}
@inforef{\\NAME\\,,,lilypond}
@cindex \\NAME\\
@end macro

@macro glossaryref{NAME}
@inforef{\\NAME\\,,lilypond/music-glossary}
@cindex \\NAME\\
@end macro

@macro inputfileref{DIR,NAME}
@file{\\DIR\\/\\NAME\\}
@end macro

@macro q{TEXT}
`\\TEXT\\'
@end macro
@end ifinfo


@iftex
@c ***** TeX *****

@macro usermanref{NAME}
@ref{\\NAME\\}@c
@end macro

@macro inputfileref{DIR,NAME}@c
@file{\\DIR\\/\\NAME\\}@c
@end macro

@macro q{TEXT}
`\\TEXT\\'
@end macro
@end iftex


@macro internalsref{NAME}
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
    (string-append  "This is the program reference for LilyPond version " (lilypond-version))

    #:children
    (list
     (music-doc-node)
     (translation-doc-node)
     (backend-doc-node)
     (all-scheme-functions-doc)
     (make <texi-node>
       #:name "Indexes"
       #:text "
@unnumbered Concept index

@printindex cp

@unnumbered Variable index

@printindex vr

@unnumbered Function index

@printindex fn

\n@bye"))))

(dump-node top-node out-port 0)
(newline (current-error-port))




