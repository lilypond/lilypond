
;;; generate-documentation.scm -- Generate documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>

;;; File entry point for generated documentation

;;; Running LilyPond on this file generates the documentation



;; We use ly-gulp because these files live in
;;
;;     PATH=$LILYPONDPREFIX/scm:<datadir>/scm
;;
(eval-string (ly-gulp-file "documentation-lib.scm"))
(eval-string (ly-gulp-file "engraver-documentation-lib.scm"))
(eval-string (ly-gulp-file "backend-documentation-lib.scm"))



;;(define no-copies #t)  ; from 490 to 410K, but doesn't look nice yet
;;
;; Also, copies of interfaces use up lots more space, but that's
;; functional because the default property values of the interfaces
;; are described...
(define no-copies #f)

(let* ((doc (string-append
 	     (document-paper "LilyPond interpretation contexts")
 	     (document-all-engravers "LilyPond engravers")
 	     (document-all-engraver-properties "LilyPond context properties")	     
 	     (document-all-elements "LilyPond backend")
 	     (document-all-interfaces "LilyPond interfaces")
	     (document-all-backend-properties "LilyPond backend properties")
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
     '(("LilyPond interpretation contexts" . "Hierarchy and grouping of Engravers")
       ("LilyPond engravers" . "Engravers create Elements")
       ("LilyPond context properties" . "context properties")       
       ("LilyPond backend" . "Detailed description of all Elements")
       ("LilyPond interfaces" . "Element Interfaces")
       ("LilyPond backend properties" . "Element properties")))

    
    
    doc
    "\n@bye")
   out))

(newline (current-error-port))
