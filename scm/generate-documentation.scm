;;; generate-documentation.scm -- Generate documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>

;;; File entry point for generated documentation

;;; Running LilyPond on this file generates the documentation

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; TODO : make modules of these!
;;;;;;;;;;;;;;;;

(define load-files '("documentation-lib.scm"
		     "function-documentation.scm"
		     "engraver-documentation-lib.scm"
		     "music-documentation-lib.scm"
		     "backend-documentation-lib.scm"
		     ))
(map ly-load load-files)


;;(define no-copies #t)  ; from 490 to 410K, but doesn't look nice yet
;;
;; Also, copies of interfaces use up lots more space, but that's
;; functional because the default property values of the interfaces
;; are described...
(define no-copies #f)

(define file-name "lilypond-internals")
(define outname (string-append file-name ".texi"))
(define out-port (open-output-file outname))

(writing-wip outname)

(display
 (string-append
  "@c -*-texinfo-*-"
  (texi-file-head "LilyPond internals" outname "(lilypond.info)")) out-port)

(define top-node
  (make <texi-node>
    #:name "Top"
    #:children
    (list
     (music-doc-node)
     (translation-doc-node)
     (backend-doc-node)
     (all-scheme-functions-doc)
     (make <texi-node>
       #:name "Index"
       #:text "
@unnumbered Concept index

@printindex cp

@unnumbered Variable index

@printindex vr

@unnumbered Function index

@printindex fn

\n@bye"

       
     )
    )))


(dump-node top-node out-port 0)
(newline (current-error-port))
