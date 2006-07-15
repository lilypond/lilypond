;;;; layout-page-tweaks.scm -- page breaking and page layout
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2006 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm layout-page-dump)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (scm paper-system)
  #:use-module (scm page)
  #:use-module (lily)
  #:export (write-page-breaks))


(define (record-tweaks what property-pairs tweaks)
  (let ((key (ly:output-def-lookup (ly:grob-layout what)
                                   'tweak-key
                                   "tweaks"))
        (when (ly:grob-property what 'when)))
    (if (not (hash-ref tweaks key))
        (hash-set! tweaks key '()))
    (hash-set! tweaks key
               (acons when property-pairs
                      (hash-ref tweaks key)))))

(define (graceless-moment mom)
  (ly:make-moment (ly:moment-main-numerator mom)
                  (ly:moment-main-denominator mom)
                  0 0))

(define (moment->skip mom)
  (let ((main (if (> (ly:moment-main-numerator mom) 0)
                  (format "\\skip 1*~a/~a"
                          (ly:moment-main-numerator mom)
                          (ly:moment-main-denominator mom))
                    ""))
        (grace (if (< (ly:moment-grace-numerator mom) 0)
                   (format "\\grace { \\skip 1*~a/~a }"
                           (- (ly:moment-grace-numerator mom))
                           (ly:moment-grace-denominator mom))
                   "")))
    (format "~a~a" main grace)))

(define (dump-tweaks out-port tweak-list last-moment)
  (if (not (null? tweak-list))
      (let* ((now (caar tweak-list))
             (diff (ly:moment-sub now last-moment))
	     (these-tweaks (cdar tweak-list))
	     (skip (moment->skip diff))
	     (line-break-str (if (assoc-get 'line-break these-tweaks #f)
				 "\\break\n"
				 ""))
	     (page-break-str (if (assoc-get 'page-break these-tweaks #f)
				 "\\pageBreak\n"
				 ""))
	     (space-tweaks (format "\\spacingTweaks #'~a\n"
				   (with-output-to-string
				     (lambda ()
				       (pretty-print
					(assoc-get 'spacing-parameters
                                                   these-tweaks '()))))))
	     (base (format "~a~a~a"
			   line-break-str
			   page-break-str
			   space-tweaks)))
        (format out-port "~a\n~a\n" skip base)
        (dump-tweaks out-port (cdr tweak-list) (graceless-moment now)))))

(define (dump-all-tweaks pages tweaks)
  (let* ((paper (ly:paper-book-paper (page-property  (car pages) 'paper-book)))
         (parser (ly:output-def-parser paper))
         (name  (format "~a-page-layout.ly"
                        (ly:parser-output-name parser)))
         (out-port (open-output-file name)))
    (ly:progress "Writing page layout to ~a" name)
    (hash-for-each
     (lambda (key val)
       (format out-port "~a = {" key)
       (dump-tweaks out-port (reverse val) (ly:make-moment 0 1))
       (display "}" out-port))
     tweaks)
    (close-port out-port)))

(define (write-page-breaks pages) 
  "Dump page breaks"
  (let ((tweaks (make-hash-table 23)))
    (define (handle-page page)
      (define index 0)
      (define music-system-heights
        (map-in-order (lambda (sys)
                        (* -1 (car (paper-system-extent sys Y))))
                      (remove (lambda (sys)
                                (ly:prob-property? sys 'is-title))
                              (page-lines page))))
      (define (handle-system sys)
        (let* ((props `((line-break . #t)
                        (spacing-parameters
                         . ((system-Y-extent . ,(paper-system-extent sys Y))
                            (system-refpoint-Y-extent . ,(paper-system-staff-extents sys))
                            (system-index . ,index)
                            (music-system-heights . ,music-system-heights)
                            (page-system-count . ,(length (page-lines page)))
                            (page-printable-height . ,(page-printable-height page)) 
                            (page-space-left . ,(page-property page 'space-left)))))))
          (if (equal? (car (page-lines page)) sys)
              (set! props (cons '(page-break . #t)
                                props)))
          (if (not (ly:prob-property? sys 'is-title))
              (record-tweaks (ly:spanner-bound (ly:prob-property sys 'system-grob) LEFT)
                            props
                            tweaks))
          (set! index (1+ index))))
      (for-each handle-system (page-lines page)))
    (for-each handle-page pages)
    (dump-all-tweaks pages tweaks)))
