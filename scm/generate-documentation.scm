
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

(let* ((doc (string-append
	    (document-paper "LilyPond interpretation contexts")
	    (document-all-engravers "LilyPond engravers")
	    (document-all-elements "LilyPond backend")))
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
       ("LilyPond backend" . "Detailed description of all Elements")))
     
    doc
    "\n@bye")
    out))

(newline (current-error-port))
