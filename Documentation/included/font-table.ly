#(set-global-staff-size 16)

#(begin

  ;; some helper functions

  (define (filter-out pred lst)
    (filter (lambda (x) (not (pred x))) lst))

  (define (filter-out-group glyph-list substring)
    (filter-out (lambda (x) (string-contains x substring)) glyph-list))

  (define (filter-out-groups glyph-list . substrings)
    (let loop ((new glyph-list) (rem substrings))
      (if (null? rem)
          new
          (loop (filter-out-group new (car rem))
                (cdr rem)))))

  (define (get-group glyph-list substring)
    (filter (lambda (x) (string-contains x substring)) glyph-list))

  (define glyph-list
    (delete ".notdef"
            (ly:otf-glyph-list (ly:system-font-load "emmentaler-20"))))

  ;;;;;;;;;

  ;; define these 3 groups first since they're
  ;; harder to get with (get-groups ...)
  (define numbers
    '("plus" "comma" "hyphen" "period"
      "zero" "one"   "two"    "three"  "four"
      "five" "six"   "seven"  "eight"  "nine"))

  (define default-noteheads
    '("noteheads.uM2" "noteheads.dM2" "noteheads.sM1"
      "noteheads.s0"  "noteheads.s1"  "noteheads.s2"))

  (define dynamics
    '("space" "f" "m" "p" "r" "s" "z"))

  ;; remove them from the glyph-list
  (for-each
    (lambda (x) (set! glyph-list (delete x glyph-list)))
    (append numbers
            default-noteheads
            dynamics))

  ;;;;;;;;;

  ;; extract ancient-music groups before extracting default
  ;; accidentals, rests, etc. to prevent duplication.
  (define vaticana    (get-group glyph-list "vaticana"))
  (define medicaea    (get-group glyph-list "medicaea"))
  (define hufnagel    (get-group glyph-list "hufnagel"))
  (define neomensural (get-group glyph-list "neomensural"))

  ;; remove neomensural before defining mensural; otherwise, searching
  ;; for "mensural" would return "neomensural" matches too.
  (set! glyph-list
    (filter-out-groups
      glyph-list
      "vaticana"
      "medicaea"
      "hufnagel"
      "neomensural"))

  ;; get the rest of the ancient-music groups
  (define mensural (get-group glyph-list "mensural"))
  (define petrucci (get-group glyph-list "petrucci"))
  (define solesmes (get-group glyph-list "solesmes"))

  ;; remove them from the glyph-list
  (set! glyph-list
    (filter-out-groups
      glyph-list
      "mensural"
      "petrucci"
      "solesmes"))

  ;; This would only get "rests.2classical".
  ;; We're leaving it with the other rests for now.
  ;; (define classical (get-group glyph-list "classical"))
  ;; (set! glyph-list (filter-out-groups glyph-list "classical"))

  ;;;;;;;;;

  ;; get everything else except noteheads.
  ;; * Some accidentals contain "slash" substring, so extract
  ;;   "accidentals" before extracting "slash" (noteheads).
  ;; * Also use "pedal." not "pedal", for example, to prevent things
  ;;   like "scripts.upedalheel" ending up in the "pedal." list.
  ;; * This doesn't apply to the ancient stuff because searching for
  ;;   "vaticana." (as an example) would miss things like
  ;;   "dots.dotvaticana"
  (define clefs       (get-group glyph-list "clefs."))
  (define timesig     (get-group glyph-list "timesig."))
  (define accidentals (get-group glyph-list "accidentals."))
  (define rests       (get-group glyph-list "rests."))
  (define flags       (get-group glyph-list "flags."))
  (define dots        (get-group glyph-list "dots."))
  (define scripts     (get-group glyph-list "scripts."))
  (define arrowheads  (get-group glyph-list "arrowheads."))
  (define brackettips (get-group glyph-list "brackettips."))
  (define pedal       (get-group glyph-list "pedal."))
  (define accordion   (get-group glyph-list "accordion."))

  ;; remove them from the glyph-list
  (set! glyph-list
    (filter-out-groups
      glyph-list
      "clefs."
      "timesig."
      "accidentals."
      "rests."
      "flags."
      "dots."
      "scripts."
      "arrowheads."
      "brackettips."
      "pedal."
      "accordion."))

  ;;;;;;;;;

  ;; get special noteheads
  (define cross    (get-group glyph-list "cross"))
  (define diamond  (get-group glyph-list "diamond"))
  (define harmonic (get-group glyph-list "harmonic"))
  (define slash    (get-group glyph-list "slash"))
  (define triangle (get-group glyph-list "triangle"))
  (define xcircle  (get-group glyph-list "xcircle"))

  (define special-noteheads
    (append cross
            diamond
            harmonic
            slash
            triangle
            xcircle))

  ;; remove special noteheads from the glyph-list
  (set! glyph-list
    (filter-out-groups
      glyph-list
      "cross"
      "diamond"
      "harmonic"
      "slash"
      "triangle"
      "xcircle"))

  ;; (lazy solution)
  ;; any remaining glyphs containing "noteheads." should be shape-notes.
  (define shape-note-noteheads (get-group glyph-list "noteheads."))

  ;; remove shape-note-noteheads from the glyph-list
  (set! glyph-list (filter-out-group glyph-list "noteheads."))

  ;;;;;;;;;

  ;; simple debug test for any glyphs that didn't make it.
  (if #f
    (if (null? glyph-list)
        (format #t "No glyphs are missing from the table.\n")
        (format #t "You missed these glyphs: ~a\n" glyph-list)))

) % end of (begin ...)

\paper {
  %% ugh. text on toplevel is a bit broken...

  oddHeaderMarkup = \markup {}
  evenHeaderMarkup = \markup {}
  oddFooterMarkup = \markup {}
  evenFooterMarkup = \markup {}
  }

\version "2.12.0"

#(define-markup-command (doc-char layout props name) (string?)
  (interpret-markup layout props
   (let* ((n (string-length name)))
     (if (> n 24)
	 ;; split long glyph names near the middle at dots
	 (let* ((middle-pos (round (/ n 2)))
		(left-dot-pos (string-rindex name #\. 0 middle-pos))
		(right-dot-pos (string-index name #\. middle-pos))
		(left-distance (if (number? left-dot-pos)
				   (- middle-pos left-dot-pos)
				   middle-pos))
		(right-distance (if (number? right-dot-pos)
				    (- right-dot-pos middle-pos)
				    middle-pos))
		(split-pos (if (> left-distance right-distance)
			       right-dot-pos
			       left-dot-pos))
		(left (substring name 0 split-pos))
		(right (substring name split-pos)))
	   (markup
	     #:pad-to-box '(0 . 36) '(-2 . 2) #:column (#:typewriter left
							#:typewriter #:concat ("  " right))
	     #:pad-to-box '(-2 . 4) '(-3.5 . 3.5) #:huge #:musicglyph name))
	 (markup
	   #:pad-to-box '(0 . 36) '(-2 . 2) #:typewriter name
	   #:pad-to-box '(-2 . 4) '(-3.5 . 3.5) #:huge #:musicglyph name)))))

#(define-markup-list-command (doc-chars layout props names) (list?)
   (define (min-length lst n)
     "(min	 (length lst) n)"
     (if (or (null? lst) (<= n 0))
	 0
	 (1+ (min-length (cdr lst) (1- n)))))
   (define (doc-chars-aux names acc)
     (let* ((n (min-length names 2))
	    (head (take names n))
	    (tail (drop names n)))
       (if (null? head)
	   (reverse! acc)
	   (doc-chars-aux tail
			 (cons (make-line-markup (map make-doc-char-markup head))
			       acc)))))
   (interpret-markup-list layout props (doc-chars-aux names (list))))
