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


(define (read-encoding-file filename)
  "Read .enc file, returning a vector of symbols."
  (let* ((raw (ly:gulp-file filename))
	 (string (regexp-substitute/global #f "%[^\n]*" raw 'pre "" 'post))
	 (start (string-index string #\[))
	 (end (string-index string #\]))
	 (ps-lst (string-tokenize (substring string (+ start 1) end)))
	 (lst (map (lambda (x) (substring x 1)) ps-lst))
	 (vector (list->vector lst)))

    vector))

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

(define-public (encoding-permutation input-encoding
				     output-encoding)

  "Contruct a permutation by applying output-encoding after input-encoding "
  (list->vector
   (map
    (lambda (chr)
      (let*
	  ((new-char (hash-ref output-encoding
			       (vector-ref input-encoding (char->integer chr)) #f)))

	;; substitute space for unknown characters.
	(if (char? new-char)
	    new-char
	    #\ )))
    (iota 256))))


(define (get-coding-from-file filename)
  "Read FILENAME, return a list containing encoding vector and table"
  
  (let*
      ((vec (read-encoding-file filename))
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

	 ;; for testing -- almost adome
	 ("adobe" . "ad.enc")
	 ("latin1" . "cork.enc")
	 
	 ;; LilyPond.
	 ("feta braces" . "feta-braces0.enc")
	 ("feta number" . "feta-nummer10.enc")
	 ("feta music" . "feta20.enc")
	 ("parmesan music" . "parmesan20.enc"))
       ))

(define (get-coding coding-name)
  (force (assoc-get coding-name coding-alist )))

(define (get-coding-vector coding-name)
  (car (get-coding coding-name)))

(define (get-coding-table coding-name)
  (cadr (get-coding coding-name)))


;;; what's this for? --hwn
(define-public (encoded-index font-coding input-coding code)
  (format (current-error-port) "CODE: ~S\n" code)
  (let* ((font (get-coding-table font-coding))
	 (in (get-coding-vector input-coding))
	 (char (vector-ref in code)))
    (format (current-error-port) "CHAR: ~S\n" char)
    (hash-ref font char)))

