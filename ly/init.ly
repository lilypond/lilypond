%% Toplevel initialisation file. 

%% switch on debugging.
#(if (and #t (defined? 'set-debug-cell-accesses!))
  (set-debug-cell-accesses! 5000))

\version "2.10.0"

\include "declarations-init.ly"


#(ly:set-option 'old-relative #f)
#(define toplevel-scores (list))
#(define toplevel-bookparts (list))
#(define output-count 0) 
#(define $defaultheader #f)
#(define version-seen #f)
#(define expect-error #f) 
#(define output-empty-score-list #f)
#(define output-suffix #f)
#(use-modules (scm clip-region))
\maininput
%% there is a problem at the end of the input file

%%
%% Above and below comments compensate for the parser's look-ahead.
%%

#(if (and (ly:get-option 'old-relative)
      (defined? 'input-file-name)
      (not (ly:get-option 'old-relative-used)))
  (old-relative-not-used-message input-file-name))%% there is a problem at the end of the input file

#(if (and (not version-seen)
      (defined? 'input-file-name))
  (version-not-seen-message input-file-name))

#(ly:set-option 'protected-scheme-parsing #f)

#(let ((book-handler (if (defined? 'default-toplevel-book-handler)
                         default-toplevel-book-handler
                         toplevel-book-handler)))
   (cond ((pair? toplevel-bookparts)
          (let ((book (ly:make-book $defaultpaper $defaultheader)))
            (map (lambda (part)
                   (ly:book-add-bookpart! book part))
                 (reverse! toplevel-bookparts))
            (set! toplevel-bookparts (list))
            ;; if scores have been defined after the last explicit \bookpart:
            (if (pair? toplevel-scores)
                (map (lambda (score)
                       (ly:book-add-score! book score))
                     (reverse! toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler parser book)))
         ((or (pair? toplevel-scores) output-empty-score-list)
          (book-handler parser (apply ly:make-book $defaultpaper
                                      $defaultheader toplevel-scores)))))

#(if (eq? expect-error (ly:parser-has-error? parser))
  (ly:parser-clear-error parser)
  (if expect-error
   (ly:parser-error parser (_ "expected error, but none found"))))
