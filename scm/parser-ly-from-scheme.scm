;;;; ly-from-scheme.scm -- parsing LilyPond music expressions from scheme
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2008  Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;;           Jan Nieuwenhuizen <janneke@gnu.org>

(define gen-lily-sym
  ;; Generate a lilyvartmpXX symbol, that may be (hopefully) unique.
  (let ((var-idx -1))
    (lambda ()
      (set! var-idx (1+ var-idx))
      (string->symbol (format #f "lilyvartmp~a"
                              (list->string (map (lambda (chr)
                                                   (integer->char (+ (char->integer #\a)
                                                                     (- (char->integer chr)
                                                                        (char->integer #\0)))))
                                                 (string->list (number->string var-idx)))))))))

(define-public (parse-string-result str parser)
  "Parse `str', which is supposed to contain a music expression."

  (ly:parser-parse-string
   parser
   (format #f "parseStringResult = \\notemode { ~a }" str))
  (ly:parser-lookup parser 'parseStringResult))

(define-public (read-lily-expression chr port)
  "Read a #{ lily music expression #} from port and return
the scheme music expression. The $ character may be used to introduce
scheme forms, typically symbols. $$ may be used to simply write a `$'
character."
  (let ((bindings '()))

    (define (create-binding! val)
      "Create a new symbol, bind it to `val' and return it."
      (let ((tmp-symbol (gen-lily-sym)))
        (set! bindings (cons (cons tmp-symbol val) bindings))
        tmp-symbol))
    
    (define (remove-dollars! form)
      "Generate a form where `$variable' and `$ value' mottos are replaced
      by new symbols, which are binded to the adequate values."
      (cond (;; $variable
             (and (symbol? form)
                  (string=? (substring (symbol->string form) 0 1) "$")
                  (not (and (<= 2 (string-length (symbol->string form)))
			    (string=? (substring (symbol->string form) 1 2) "$"))))
             (create-binding! (string->symbol (substring (symbol->string form) 1))))
            (;; atom
             (not (pair? form)) form)
            (;; ($ value ...)
             (eqv? (car form) '$)
             (cons (create-binding! (cadr form)) (remove-dollars! (cddr form))))
            (else ;; (something ...)
             (cons (remove-dollars! (car form)) (remove-dollars! (cdr form))))))
    
    (let ((lily-string (call-with-output-string
                        (lambda (out)
                          (do ((c (read-char port) (read-char port)))
                              ((and (char=? c #\#)
                                    (char=? (peek-char port) #\})) ;; we stop when #} is encoutered
                               (read-char port))
                            (cond
                             ;; a $form expression
                             ((and (char=? c #\$) (not (char=? (peek-char port) #\$)))
                              (format out "\\~a" (create-binding! (read port))))
                             ;; just a $ character
                             ((and (char=? c #\$) (char=? (peek-char port) #\$))
                              ;; pop the second $
                              (display (read-char port) out))
                             ;; a #scheme expression
                             ((char=? c #\#)
                              (let ((expr (read port)))
                                (format out "#~s" (if (eq? '$ expr)
                                                      (create-binding! (read port))
                                                      (remove-dollars! expr)))))
                             ;; other caracters
                             (else
                              (display c out))))))))
      `(let ((parser-clone (ly:parser-clone parser)))
         ,@(map (lambda (binding)
                  `(ly:parser-define! parser-clone ',(car binding) ,(cdr binding)))
                (reverse bindings))
         (parse-string-result ,lily-string parser-clone)))))

(read-hash-extend #\{ read-lily-expression)
