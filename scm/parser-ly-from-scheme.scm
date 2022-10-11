;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2022  Nicolas Sceaux  <nicolas.sceaux@free.fr>
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

#|
This file implements the #{ ... #} reader extension that allows embedding
values in LilyPond syntax within Scheme code.  At read time, the
implementation of this reader extension outputs a call to
read-lily-expression-internal.  At evaluation time,
read-lily-expression-internal will clone the current parser to parse the
value, and return it.

Complications arise because of the Scheme code that this LilyPond might in
turn embed.  We want the local variables from outer Scheme code to be available
to that inner Scheme code, as in

(let ((mus ...))  #{ \transpose c d #mus #})

The "obvious" thing to do would be to use Guile's local-eval facility, which
allows capturing a lexical environment and later evaluating expressions in
that environment.  This implementation is different.  When reading #{ #}, we
catch Scheme expressions.  Say we encounter an expression <expr>.  We will
output code that passes (lambda () <expr>) to read-lily-expression-internal,
associated with the position of this <expr> in the source.  When the parser
wants to evaluate this bit of Scheme code, it will just call that thunk
(0-argument function).

While this implementation is an artifact of history, it is being kept because it
has features that would not be possible with local-eval.  For example, it plays
nicely with quasiquotation. Example from David K.:

#(define-macro (pattern args result)
`(define-music-function (parser location ,@args) ,(make-list (length args) 'ly:music?)
#{ $@(list ,@result) #}))

$(pattern (A B C D) (A B D A C D)) { a' } { b' } { c'' } { d'' }

This works because quasiquoting a Scheme expression that includes a #{ #} that
in turn includes an inner Scheme expression, and unquoting in that inner Scheme
expression indeed performs the substitution.  The code (lambda () <expr>) is
generated and includes an unquote form, which is replaced during macro expansion.
In more low-level terms, this means that the macro expansion phase of the outer
Scheme code also processes the inner Scheme code.  That would not be possible with
local-eval.

On the other hand, it does have the shortcoming that something like

##{ \paper { #(define foo 'bar) } #}

is not possible, since (lambda () (define foo 'bar)) is not valid.
|#


(define (read-lily-expression-internal lily-string filename line closures)
  "Direct the lilypond parser to parse LILY-STRING, using FILENAME,
LINE for diagnostics. CLOSURES holds an alist of (BYTE-OFFSET . DATA),
representing embedded Scheme in LILY-STRING"
  (let* ((clone (ly:parser-clone closures (*location*)))
         (result (ly:parse-string-expression clone lily-string filename line)))
    (if (ly:parser-has-error? clone)
        (ly:parser-error (G_ "error in #{ ... #}") (*location*)))
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
                                (let ((p (ftell out)))
                                  ;; Need to synchronize the
                                  ;; copycat port with
                                  ;; line/column data
                                  (set-port-line! copycat
                                                  (port-line port))
                                  (set-port-column! copycat
                                                    (port-column port))
                                  ;; @ splices a list
                                  (let ((is-multiple (char=? (peek-char port) #\@)))
                                    (when is-multiple
                                      ;; discard @ before reading Scheme expression
                                      (read-char copycat))
                                    (let ((expr (read copycat)))
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
                                                (cons `(cons ,p
                                                             (lambda ()
                                                               ,(if is-multiple
                                                                    `(apply values ,expr)
                                                                    expr)))
                                                      closures)))))))
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
    (list '(@@ (lily) read-lily-expression-internal)
          lily-string filename line (cons 'list (reverse! closures)))))

(read-hash-extend #\{ read-lily-expression)
