;;;;  predefined-fretboards.scm
;;;;
;;;;  source file of the GNU LilyPOnd music typesetter
;;;;
;;;; (c) 2008 Carl D. Sorensen <c_sorensen@byu.edu>


(define-public (parse-terse-string terse-definition)
"Parse a fret-diagram-terse definition string @code{terse-definition} and
return a marking list, which can be used as with a fretboard grob."
   (cdr (fret-parse-terse-definition-string (list '()) terse-definition)))

(define-public (get-chord-shape shape-code base-chord-shapes)
"Return the chord shape associated with key @code{shape-code} in
the alist @code{base-chord-shapes}."
   (assoc-get shape-code base-chord-shapes #f))

(define-public (offset-fret fret-offset terse-string)
"Add @code{fret-offset} to each fret indication in @code{terse-string}
and return the resulting fret-diagram-terse definition string."

   (define (split-fretstring fret-string)
     (map (lambda (x) (split-item x))
          (string-split fret-string #\sp )))

   (define (split-item item-string)
     (string-split item-string #\- ))

   (define (split-terse-string terse-string)
      (let ((long-list
               (string-split terse-string #\;)))
        (map (lambda (x) (split-fretstring x))
           (list-head long-list (1- (length long-list))))))

   (define (join-terse-string terse-string-list)
     (string-join
        (map (lambda (x) (join-fretstring x)) terse-string-list)
        ";" 'suffix))

   (define (join-item item-list)
     (string-join item-list "-" ))

   (define (join-fretstring fretstring-list)
     (string-join
       (map (lambda (x) (join-item x)) fretstring-list)
       " " ))

  (define (add-item-fret-offset fret-offset item-list)
     (let ((fretval (string->number (car item-list))))
       (if fretval
           (cons (number->string (+ fretval fret-offset))
                 (cdr item-list))
           item-list)))

  (define (add-fretstring-fret-offset fret-offset fretstring-list)
    (map (lambda (x) (add-item-fret-offset fret-offset x))
         fretstring-list))

  (define (add-terse-fret-offset fret-offset terse-string-list)
    (map (lambda (x) (add-fretstring-fret-offset fret-offset x))
         terse-string-list))

;; body
  (join-terse-string
    (add-terse-fret-offset
      fret-offset
      (split-terse-string terse-string))))

