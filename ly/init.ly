%% Toplevel initialisation file.

%% switch on debugging.
#(if (and #t (defined? 'set-debug-cell-accesses!))
  (set-debug-cell-accesses! 5000))

\version "2.19.22"

#(if (guile-v2)
  (use-modules (ice-9 curried-definitions)))

#(session-initialize
  (lambda ()
   ;; we can't use ly:parser-include-string here since that does not
   ;; actually do any parsing but merely switches inputs, so the
   ;; session saved by the session initializer after calling this
   ;; function has not actually started.  A parser clone, in contrast,
   ;; can run and complete synchronously and shares the module with
   ;; the current parser.
   (ly:parser-parse-string (ly:parser-clone)
    "\\include \"declarations-init.ly\"")))

#(note-names-language default-language)

#(define toplevel-scores (list))
#(define toplevel-bookparts (list))
#(define $defaultheader #f)
#(define $current-book #f)
#(define $current-bookpart #f)
#(define version-seen #f)
#(define expect-error #f)
#(define output-empty-score-list #f)
#(define output-suffix #f)

#(use-modules (scm clip-region))
#(use-modules (srfi srfi-1))
#(use-modules (ice-9 pretty-print))

$(if (ly:get-option 'include-settings)
  (ly:parser-include-string
    (format #f "\\include \"~a\"" (ly:get-option 'include-settings))))

\maininput
%% there is a problem at the end of the input file

%%
%% Above and below comments compensate for the parser's look-ahead.
%%

#(if (and (not version-seen)
      (defined? 'input-file-name))
  (version-not-seen-message input-file-name))

#(ly:set-option 'protected-scheme-parsing #f)

#(let ((book-handler (if (defined? 'default-toplevel-book-handler)
                         default-toplevel-book-handler
                         toplevel-book-handler)))
   (cond ((pair? toplevel-bookparts)
          (let ((book (ly:make-book $defaultpaper $defaultheader)))
            (for-each (lambda (part)
                        (ly:book-add-bookpart! book part))
                      (reverse! toplevel-bookparts))
            (set! toplevel-bookparts (list))
            ;; if scores have been defined after the last explicit \bookpart:
            (if (pair? toplevel-scores)
                (for-each (lambda (score)
                            (ly:book-add-score! book score))
                          (reverse! toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler book)))
         ((or (pair? toplevel-scores) output-empty-score-list)
          (let ((book (apply ly:make-book $defaultpaper
                             $defaultheader toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler book)))))

#(if (eq? expect-error (ly:parser-has-error?))
  (ly:parser-clear-error)
  (if expect-error
   (ly:parser-error (_ "expected error, but none found"))))
