;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020  Nicolas Sceaux  <nicolas.sceaux@free.fr>
;;;;           Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define (read-lily-expression-internal lily-string filename line closures)
  "Direct the lilypond parser to parse LILY-STRING, using FILENAME,
LINE for diagnostics. CLOSURES holds an alist of (BYTE-OFFSET . DATA),
representing embedded Scheme in LILY-STRING"
  (let* ((clone (ly:parser-clone closures (*location*)))
         (result (ly:parse-string-expression clone lily-string filename line)))
    (if (ly:parser-has-error? clone)
        (ly:parser-error (_ "error in #{ ... #}") (*location*)))
    result))

(define-public (read-lily-expression chr port)
  "Read a lilypond music expression enclosed within @code{#@{} and @code{#@}}
from @var{port} and return the corresponding Scheme music expression.
@samp{$} and @samp{#} introduce immediate and normal Scheme forms."
  (let* ((closures '())
         (filename (port-filename port))
         (line (port-line port))

         ;; TODO: this creates ports (make-soft-port, call-with-output-string).
         ;; we should clarify what input-encoding these ports use. It's also likely
         ;; that embedded Scheme in #{ #} is broken when mixed with UTF-8.
         (lily-string (call-with-output-string
                       (lambda (out)
                         (define (copy-char)
                           (let ((x (read-char port)))
                             (write-char x out)
                             x))
                         (let ((copycat
                                (make-soft-port
                                 (vector #f #f #f copy-char #f)
                                 "r")))
                           (set-port-filename! copycat filename)
                           (do ((c (read-char port) (read-char port)))
                               ((and (char=? c #\#)
                                     (char=? (peek-char port) #\}))
                                ;; we stop when #} is encountered
                                (read-char port))
                             (write-char c out)
                             ;; a #scheme or $scheme expression
                             (case c
                               ((#\# #\$)
                                ;; These characters enter a Scheme
                                ;; expression to be captured in a
                                ;; closure
                                (let* ((p (ftell out))
                                       (expr
                                        (begin
                                          ;; Need to synchronize the
                                          ;; copycat port with
                                          ;; line/column data
                                          (set-port-line! copycat
                                                          (port-line port))
                                          (set-port-column! copycat
                                                            (port-column port))
                                          ;; @ splices a list
                                          (if (char=? (peek-char port) #\@)
                                              (read-char copycat))
                                          (read copycat))))
                                  ;; kill unused lookahead, it has been
                                  ;; written out already
                                  (drain-input copycat)
                                  ;; only put symbols and non-quote
                                  ;; lists into closures -- constants
                                  ;; don't need lexical environments
                                  ;; for evaluation.
                                  (if (or (symbol? expr)
                                          (and (pair? expr)
                                               (not (eq? 'quote (car expr)))))
                                      (set! closures
                                            (cons `(cons ,p (lambda () ,expr))
                                                  closures)))))
                               ((#\")
                                ;; A LilyPond string is ended by the
                                ;; quote character unless the quote
                                ;; character itself is escaped.  Note:
                                ;; \"xxx" is a valid LilyPond
                                ;; construct as well, so we don't need
                                ;; to track leading backslashes here.
                                (do ((c (copy-char) (copy-char)))
                                    ((char=? c #\"))
                                  (if (char=? c #\\)
                                      (copy-char))))
                               ((#\%)
                                ;; LilyPond comments end at the end of
                                ;; line unless they have the form
                                ;; %{...%}
                                (case (copy-char)
                                  ((#\{)
                                   (do ((c (copy-char) (copy-char)))
                                       ((and (char=? c #\%) (peek-char port) #\})
                                        (copy-char))))
                                  ((#\nl) #f) ; empty comment
                                  (else
                                   (do ((c (copy-char) (copy-char)))
                                       ((char=? c #\nl)))))))))))))
    (list (cond-expand
           (guile-2 '(@@ (lily) read-lily-expression-internal))
           (else read-lily-expression-internal))
          lily-string filename line (cons 'list (reverse! closures)))))

(read-hash-extend #\{ read-lily-expression)
