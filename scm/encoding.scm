;;;; encoding.scm -- font encoding
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>

;; WIP
;; encoding.ly:
;; #(display (reencode-string "adobe" "latin1" "hellö fóebär"))
;;


(define-public (read-encoding-file filename)
  "Read .enc file, return as a vector of symbols."
  (let* ((raw (ly:kpathsea-gulp-file filename))
	 (string (regexp-substitute/global #f "%[^\n]*" raw 'pre "" 'post))
	 (start (string-index string #\[))
	 (end (string-index string #\]))
	 (ps-lst (string-tokenize (substring string (+ start 1) end)))
	 (lst (map (lambda (x) (substring x 1)) ps-lst)))
    (list->vector lst)))

(define (make-encoding-table encoding-vector)
  "Return a hash table mapping names to chars. ENCODING-VECTOR is a
vector of symbols."

  (let* ((h (make-hash-table 256)))
    
    (for-each
     (lambda (i)
       (hash-set! h (vector-ref encoding-vector i)
		  (integer->char i)))
     (iota 256))

    h))

(define-public (reencode-string permutation str)
  "Apply PERMUTATION, a vector of [0..256) -> char, to STR"
  (string-map (lambda (chr)
		(vector-ref permutation (char->integer chr)))
	      str))

(define-public (make-encoding-permutation input-encoding output-encoding)
  "Contruct a permutation by applying output-encoding after input-encoding "
  (list->vector
   (map
    (lambda (byte)
      (let ((new-char (hash-ref
		       output-encoding (vector-ref input-encoding byte) #f)))
		       ;;input-encoding (vector-ref output-encoding byte) #f)))

	;; substitute space for unknown characters.
	(if (char? new-char)
	    new-char
	    #\ )))
    (iota 256))))


(define (get-coding-from-file filename)
  "Read FILENAME, return a list containing encoding vector and table"
   (let* ((vec (read-encoding-file filename))
	  (tab (make-encoding-table vec)))
    (list vec tab)))

;; coding-alist maps NAME -> (list VECTOR TAB)
(define coding-alist
  
  (map (lambda (x)
	 (cons (car x)
	       (delay (get-coding-from-file (cdr x)))))
       
       '(
	 ;; teTeX
	 ("TeX typewriter text" . "09fbbfac.enc") ;; cmtt10
	 ("TeX math symbols" . "10037936.enc") ;; cmbsy
	 ("ASCII caps and digits" . "1b6d048e.enc") ;; cminch
	 ("TeX math italic" . "aae443f0.enc")  ;; cmmi10
	 ("TeX extended ASCII" . "d9b29452.enc")
	 ("TeX text" . "f7b6d320.enc")
	 ("TeX text without f-ligatures" . "0ef0afca.enc")
	 ("Extended TeX Font Encoding - Latin" . "tex256.enc")
	 
	 ("T1" . "tex256.enc")

	 ;; FIXME: find full Adobe; for testing -- almost Adobe:
	 ("adobe" . "ad.enc")

	 ("latin1" . "cork.enc")
	 
	 ;; LilyPond.
	 ("feta braces" . "feta-braces-a.enc")
	 ("feta number" . "feta-nummer10.enc")
	 ("feta music" . "feta20.enc")
	 ("parmesan music" . "parmesan20.enc"))
       ))

(define (get-coding coding-name)
  (force (assoc-get coding-name coding-alist )))

(define-public (get-coding-vector coding-name)
  (car (get-coding coding-name)))

(define-public (get-coding-table coding-name)
  (cadr (get-coding coding-name)))


;;; JUNKME
;;; what's this for? --hwn
;; (define-public (encoded-index font-coding input-coding code)
;; This was used by simplistic first incarnation of reencode-string --jcn
