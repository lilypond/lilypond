
;;; generate-interface-doc.scm -- Generate list of all intefaces, for refman
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
 	     (document-all-interfaces "Full Grob interface list")
	     )
       )
       (name "interfaces")
       (outname (string-append name ".itexi"))
       (out (open-output-file outname)))

  (writing-wip outname)
  (display 
   (string-append
    ;;(itexi-file-head
    ;; 
    ;; ;; we can't use (dir) and top if we're included by lilypond.tely
    ;; "Grob interfaces" name ""
    ;; '(
    ;;   ("Grob interfaces" . "Grob Interfaces")
    ;;   ))
    doc
    "\n")
   out))

(newline (current-error-port))
