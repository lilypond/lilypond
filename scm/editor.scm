;;;; editor.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2005 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm editor))

(use-modules
 (ice-9 regex))

(define editor-command-template-alist
  '(("emacs" .  "emacsclient --no-wait +%(line)s:%(column)s %(file)s")
    ("gvim" . "gvim --remote +:%(line)s:norm%(column)s %(file)s")
    ("nedit" . "nc -noask +%(line)s %(file)s")
    ("gedit" . "gedit +%(line)s %(file)s")
    ("jedit" . "jedit %(file)s +line:%(line)s")
    ("lilypad" . "lilypad +%(line)s:%(column)s %(file)s")))

(define (get-editor)
  (or (getenv "LYEDITOR")
      (getenv "XEDITOR")
      (getenv "EDITOR")
      "emacs"))

(define (get-command-template alist editor)
  (define (get-command-template-helper)
    (if (null? alist)
	(if (string-match "%\\(file\\)s" editor)
	    editor
	    (string-append editor " %(file)s"))
	(if (string-match (caar alist) editor)
	    (cdar alist)
	    (get-command-template (cdr alist) editor))))
  (if (string-match "%\\(file\\)s" editor)
      editor
      (get-command-template-helper)))

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

(define (slashify x)
 (if (string-index x #\/)
     x
     (re-sub "\\\\" "/" x)))

(define-public (get-editor-command file-name line column)
  (let* ((editor (get-editor))
	 (template (get-command-template editor-command-template-alist editor))
	 (command
	  (re-sub "%\\(file\\)s" (format #f "~S" file-name)
		  (re-sub "%\\(line\\)s" (format #f "~a" line)
			  (re-sub "%\\(column\\)s" (format #f "~a" column)
				  (slashify template))))))
    command))
