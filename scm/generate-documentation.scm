;;; generate-documentation.scm -- Generate documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>

;;; File entry point for generated documentation

;;; Running LilyPond on this file generates the documentation




;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; TODO : make modules of these!
;;;;;;;;;;;;;;;;

(define load-files '("documentation-lib.scm"
		     "function-documentation.scm"
		     "engraver-documentation-lib.scm"
		     "music-documentation-lib.scm"
		     "backend-documentation-lib.scm"
		     ))

(map load-from-path load-files)


;;(define no-copies #t)  ; from 490 to 410K, but doesn't look nice yet
;;
;; Also, copies of interfaces use up lots more space, but that's
;; functional because the default property values of the interfaces
;; are described...
(define no-copies #f)



(let* ((doc (string-append
	     (document-music "music properties") 
 	     (document-paper "interpretation contexts")
 	     (document-all-engravers "engravers")
	     (document-all-engraver-properties "context properties")
 	     (document-all-grobs "grob overview")
 	     (document-all-interfaces "interfaces")

	     (node "backend properties")
	     (texi-section 1 "backend properties" #f)

	     (document-all-backend-properties "backend properties")

	     (node "function documentation")
	     (texi-section 1 "function documentation" #f)

	     (document-all-scheme-functions)
	     
	     )
       )
       (name "lilypond-internals")
       (outname (string-append name ".texi"))
       (out (open-output-file outname)))

  (writing-wip outname)
  (display 
   (string-append
    (texi-file-head
     
     ;; we can't use (dir) and top if we're included by lilypond.tely
     "LilyPond internals" name "(lilypond.info)"
     '(
       ("music properties" . "properties for Music representation")
       ("interpretation contexts" . "Hierarchy and grouping of Engravers")
       ("engravers" . "Engravers create Grobs")
       ("context properties" . "context properties")       
       ("grob overview" . "Detailed description of all Grobs")
       ("interfaces" . "Grob Interfaces")
       ("backend properties" . "Grob properties")
       ("function documentation" . "All embedded functions")
       ("Index" . "index")
       ))

    
    
    doc

    "@node Index
@unnumbered Concept index

@printindex cp

@unnumbered Variable index

@printindex vr

@unnumbered Function index

@printindex fn

"

    
    "\n@bye")
   out))

(newline (current-error-port))
