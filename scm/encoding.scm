;;;; encoding.scm -- font encoding
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>

;; WIP
;; cp /usr/share/texmf/dvips/base/*.enc mf/out
;; cp /usr/share/texmf/dvips/tetex/*.enc mf/out
;; encoding.ly:
;; #(display (reencode-string "adobe" "latin1" "hellö fóebär"))
;;

(define coding-file-alist
  ;; teTeX
  '(("TeX typewriter text" . "09fbbfac.enc") ;; cmtt10
    ("TeX math symbols" . "10037936.enc") ;; cmbsy
    ("ASCII caps and digits" . "1b6d048e") ;; cminch
    ("TeX math italic" . "aae443f0.enc")  ;; cmmi10
    ("TeX extended ASCII" . "d9b29452.enc")
    ("TeX text" . "f7b6d320.enc")
    ("TeX text without f-ligatures" . "0ef0afca.enc")
    ("Extended TeX Font Encoding - Latin" . "tex256.enc")
    
    ("T1" . "tex256.enc")

    ;; for testing -- almost adome
    ("adobe" . "ad.enc")
    ("latin1" . "cork.enc")
    
    ;; LilyPond.
    ("feta braces" . "feta-braces0.enc")
    ("feta number" . "feta-nummer10.enc")
    ("feta music" . "feta20.enc")
    ("parmesan music" . "parmesan20.enc")))

(define encoding-alist '())

(define (read-coding-file coding)
  (let* ((raw (ly:gulp-file (assoc-get coding coding-file-alist)))
	 ;;(raw (ly:gulp-file "f7b6d320.enc"))
	 (string (regexp-substitute/global #f "%[^\n]*" raw 'pre "" 'post))
	 (start (string-index string #\[))
	 (end (string-index string #\]))
	 (ps-lst (string-tokenize (substring string (+ start 1) end)))
	 (lst (map (lambda (x) (substring x 1)) ps-lst))
	 (vector (list->vector lst))
	 (table (make-hash-table 256)))
    (do ((i 0 (+ i 1)))
	((>= i 256))
      (hash-create-handle! table (vector-ref vector i) i))
    (let ((entry (cons coding (cons vector table))))
      (set! encoding-alist (append (list entry) encoding-alist))
      (cdr entry))))

(define (get-coding-table coding)
  (let ((entry (assoc-get coding encoding-alist #f)))
    (if entry (cdr entry)
	(cdr (read-coding-file coding)))))

(define (get-coding-vector coding)
  (let ((entry (assoc-get coding encoding-alist #f)))
    (if entry (car entry)
	(car (read-coding-file coding)))))

(define-public (encoded-index font-coding input-coding code)
  (format (current-error-port) "CODE: ~S\n" code)
  (let* ((font (get-coding-table font-coding))
	 (in (get-coding-vector input-coding))
	 (char (vector-ref in code)))
    (format (current-error-port) "CHAR: ~S\n" char)
    (hash-ref font char)))

(define-public (reencode-string font-coding input-coding s)
  ;; ughr?
  (list->string
   (map integer->char 
	(map (lambda (x) (encoded-index font-coding input-coding x))
	     ;;(map char->integer (string->list s))))))
	     (map char->integer (plain-string->list s))))))

