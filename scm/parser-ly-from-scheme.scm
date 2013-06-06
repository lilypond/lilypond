;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2012  Nicolas Sceaux  <nicolas.sceaux@free.fr>
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

(define-public (read-lily-expression chr port)
  "Read a lilypond music expression enclosed within @code{#@{} and @code{#@}}
from @var{port} and return the corresponding Scheme music expression.
@samp{$} and @samp{#} introduce immediate and normal Scheme forms."
  (let* ((closures '())
         (filename (port-filename port))
         (line (port-line port))
         (lily-string (call-with-output-string
                       (lambda (out)
                         (let ((copycat
                                (make-soft-port
                                 (vector #f #f #f
                                         (lambda ()
                                           (let ((x (read-char port)))
                                             (write-char x out)
                                             x)) #f)
                                 "r")))
                           (set-port-filename! copycat filename)
                           (do ((c (read-char port) (read-char port)))
                               ((and (char=? c #\#)
                                     (char=? (peek-char port) #\}))
                                ;; we stop when #} is encountered
                                (read-char port))
                             (write-char c out)
                             ;; a #scheme or $scheme expression
                             (if (or (char=? c #\#) (char=? c #\$))
                                 (let* ((p (ftell out))
                                        (expr
                                         (begin
                                           (set-port-line! copycat
                                                           (port-line port))
                                           (set-port-column! copycat
                                                             (port-column port))
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
                                                   closures)))))))))))
    (define (embedded-lilypond parser lily-string filename line
                               closures location)
      (let* ((clone (ly:parser-clone parser closures location))
             (result (ly:parse-string-expression clone lily-string
                                                 filename line)))
        (if (ly:parser-has-error? clone)
            (ly:parser-error parser (_ "error in #{ ... #}")))
        result))
    (list embedded-lilypond
          'parser lily-string filename line
          (cons 'list (reverse! closures))
          'location)))

(read-hash-extend #\{ read-lily-expression)
