;;;; ly-from-scheme.scm -- parsing LilyPond music expressions from scheme
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2000--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;;;                  Jan Nieuwenhuizen <janneke@gnu.org>


(define gen-lily-sym
  ;; Generate a lilyvartmpXX symbol, that may be (hopefully) unique.
  (let ((var-idx -1))
    (lambda ()
      (set! var-idx (1+ var-idx))
      (string->symbol (format #f "lilyvartmp~a"
                              (list->string (map (lambda (chr)
                                                   (integer->char (+ (char->integer #\a) (- (char->integer chr)
                                                                                            (char->integer #\0)))))
                                                 (string->list (number->string var-idx)))))))))

(define-public (ly:parse-string-result str module)
  "Parse `str', which is supposed to contain a music expression."
  (let ((music-sym (gen-lily-sym)))
    (ly:parser-parse-string
     parser
     (format #f "
~a = { ~a }
#(ly:export '~a)
#(module-define! (resolve-module '~a) '~a ~a)
"
             music-sym str music-sym (module-name module) music-sym music-sym))
  (eval `,music-sym module)))

(define-public (read-lily-expression chr port)
  "Read a #{ lily music expression #} from port and return
the scheme music expression. The $ character may be used to introduce
scheme forms, typically symbols. $$ may be used to simply write a `$'
character."
  (let* ((format-args '())
         (lily-string (with-output-to-string
                        (lambda ()
                          (do ((c (read-char port) (read-char port)))
                              ((and (char=? c #\#)
                                    (char=? (peek-char port) #\}))
                               (read-char port))
                            (cond ((and (char=? c #\$)
                                        (not (char=? (peek-char port) #\$)))
                                   ;; a $variable
                                   (display "~a")
                                   (set! format-args (cons (read port) 
format-args)))
                                  ((and (char=? c #\$)
                                        (char=? (peek-char port) #\$))
                                   ;; just a $ character
                                   (display (read-char port)))
                                  (else
                                   ;; other caracters
                                   (display c))))))))
   `(ly:parse-string-result (format #f ,lily-string ,@(reverse! format-args))
                            (current-module))))

(read-hash-extend #\{ read-lily-expression)
