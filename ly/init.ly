%% Toplevel initialisation file. 

%% switch on debugging.
#(if (and #t (defined? 'set-debug-cell-accesses!))
  (set-debug-cell-accesses! 5000))

\version "2.15.18"

#(if (not (pair? lilypond-declarations))
     (ly:parser-include-string parser
			       "\\include \"declarations-init.ly\""))

%% We need to save the variables of the current module along with
%% their values: functions defined in the module might refer to the
%% variables

#(if lilypond-declarations
     (if (pair? lilypond-declarations)
	 (begin
	   (for-each
	    (lambda (p)
	      (let ((var (cadr p))
		    (val (cddr p)))
		(variable-set! var
			       (if (ly:output-def? val)
				   (ly:output-def-clone val)
				   val))
		(module-add! (current-module) (car p) var)))
	    lilypond-declarations)
	   (note-names-language parser default-language))
	 (module-for-each
	  (lambda (s v)
	    (let ((val (variable-ref v)))
	      (if (not (ly:lily-parser? val))
		  (set! lilypond-declarations
			(cons
			 (cons*
			  s v
			  (if (ly:output-def? val)
			      (ly:output-def-clone val)
			      val))
			 lilypond-declarations)))))
	  (current-module))))

#(ly:set-option 'old-relative #f)
#(define toplevel-scores (list))
#(define toplevel-bookparts (list))
#(define $defaultheader #f)
#(define $current-book #f)
#(define $current-bookpart #f)
#(define version-seen #f)
#(define expect-error #f)
#(define output-empty-score-list #f)
#(define output-suffix #f)
#(hash-clear! default-fret-table)
#(hash-clear! chord-shape-table)
#(hash-clear! musicQuotes)

#(use-modules (scm clip-region))
#(use-modules (srfi srfi-1))

$(if (ly:get-option 'include-settings)
  (ly:parser-include-string parser
    (format #f "\\include \"~a\"" (ly:get-option 'include-settings))))

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
          (let ((book (apply ly:make-book $defaultpaper 
                             $defaultheader toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler parser book)))))

#(if (eq? expect-error (ly:parser-has-error? parser))
  (ly:parser-clear-error parser)
  (if expect-error
   (ly:parser-error parser (_ "expected error, but none found"))))
