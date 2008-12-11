;;
;; kpathsea.scm -- implement kpath support using command-line
;; 
;;
;; source file of the GNU LilyPond music typesetter
;;
;; (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;

(define-module (scm kpathsea))

(use-modules (ice-9 popen)
	     (lily)
	     (ice-9 rdelim))



(define-public (ly:kpathsea-find-file name)
  (let*
      ((cmd  (format #f "kpsewhich ~a"
		     (sanitize-command-option name)))
       (unused (if (ly:get-option 'verbose)
		   (ly:message "Running ~a" cmd)))
       (pipe (open-input-pipe cmd))
       (answer (read-line pipe)))

    (if (string? answer)
	answer
	#f)))
			    
(define-public (ly:kpathsea-expand-variable var)
  (let*
      ((cmd (format #f "kpsexpand '$'~a"
		    (sanitize-command-option var)))
       (unused (if (ly:get-option 'verbose)
		   (ly:message "Running ~a" cmd)))
       (pipe (open-input-pipe cmd))
       (answer (read-line pipe)))

    (if (string? answer)
	answer #f)))

;; Test:

;;(display (ly:kpathsea-find-file "cmr10.tfm"))
;;
;;(display (ly:kpathsea-expand-variable "TEXMF"))

