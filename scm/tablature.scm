;;;; tablature.scm
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2009 Marc Hohl <marc@hohlart.de>

;; default tunings for common string instruments
;; guitar tunings
(define-public guitar-tuning '(4 -1 -5 -10 -15 -20))
(define-public guitar-seven-string-tuning '(4 -1 -5 -10 -15 -20 -25))
(define-public guitar-drop-d-tuning '(4 -1 -5 -10 -15 -22))
(define-public guitar-open-g-tuning '(2 -1 -5 -10 -17 -22))
(define-public guitar-open-d-tuning '(2 -3 -6 -10 -15 -22))
(define-public guitar-dadgad-tuning '(2 -3 -7 -10 -15 -22))
(define-public guitar-lute-tuning '(4 -1 -6 -10 -15 -20))
(define-public guitar-asus4-tuning '(4 -3 -8 -10 -15 -20))
;; bass tunings
(define-public bass-tuning '(-17 -22 -27 -32))
(define-public bass-four-string-tuning '(-17 -22 -27 -32))
(define-public bass-drop-d-tuning '(-17 -22 -27 -34))
(define-public bass-five-string-tuning '(-17 -22 -27 -32 -37))
(define-public bass-six-string-tuning '(-12 -17 -22 -27 -32 -37))
;; mandolin
(define-public mandolin-tuning '(16 9 2 -5))
;; tunings for 5-string banjo
(define-public banjo-open-g-tuning '(2 -1 -5 -10 7))
(define-public banjo-c-tuning '(2 -1 -5 -12 7))
(define-public banjo-modal-tuning '(2 0 -5 -10 7))
(define-public banjo-open-d-tuning '(2 -3 -6 -10 9))
(define-public banjo-open-dm-tuning '(2 -3 -6 -10 9))
;; convert 5-string banjo tuning to 4-string by removing the 5th string
(define-public (four-string-banjo tuning)
  (reverse (cdr (reverse tuning))))

;; for more control over glyph-name calculations,
;; we use a custom callback for tab noteheads
;; which will ignore 'style = 'do
(define-public (tab-note-head::calc-glyph-name grob)
  (let ((style (ly:grob-property grob 'style)))

    (case style
      ((cross) "2cross"))))

;; ensure we only call notehead callback when
;; 'style = 'cross
(define-public (tab-note-head::whiteout-if-style-set grob)
  (let ((style (ly:grob-property grob 'style)))

    (if (and (symbol? style)
             (eq? style 'cross))
        (stencil-whiteout (ly:note-head::print grob))
        (ly:text-interface::print grob))))

;; definitions for the "moderntab" clef:
;; the "moderntab" clef will be added to the list of known clefs,
;; so it can be used as any other clef: \clef "moderntab"
(add-new-clef "moderntab" "markup.moderntab" 0 0 0)

;; define sans serif-style tab-Clefs as a markup:
(define-builtin-markup-command (customTabClef
                                layout props num-strings staff-space)
  (integer? number?)
  music
  ()
  "Draw a tab clef sans-serif style."
  (define (square x) (* x x))
  (let* ((scale-factor (/ staff-space 1.5))
         (font-size (- (* num-strings 1.5 scale-factor) 7))
         (base-skip (* (square (+ (* num-strings 0.195) 0.4)) scale-factor)))

    (interpret-markup layout props
                      (markup #:vcenter #:bold
                              #:override (cons 'font-family 'sans)
                              #:fontsize font-size
                              #:override (cons 'baseline-skip base-skip)
                              #:left-align #:center-column ("T" "A" "B")))))

;; this function decides which clef to take
(define-public (clef::print-modern-tab-if-set grob)
  (let ((glyph (ly:grob-property grob 'glyph)))

    ;; which clef is wanted?
    (if (string=? glyph "markup.moderntab")
        ;; if it is "moderntab", we'll draw it
        (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
               (line-count (ly:grob-property staff-symbol 'line-count))
               (staff-space (ly:staff-symbol-staff-space grob)))

          (grob-interpret-markup grob (make-customTabClef-markup line-count
                                                                 staff-space)))
        ;; otherwise, we simply use the default printing routine
        (ly:clef::print grob))))

;; if stems are drawn, it is nice to have a double stem for
;; (dotted) half notes to distinguish them from quarter notes:
(define-public (tabvoice::draw-double-stem-for-half-notes grob)
  (let ((stem (ly:stem::print grob)))

    ;; is the note a (dotted) half note?
    (if (= 1 (ly:grob-property grob 'duration-log))
        ;; yes -> draw double stem
        (ly:stencil-combine-at-edge stem X RIGHT stem 0.5)
        ;; no -> draw simple stem
        stem)))

;; as default, the glissando line between fret numbers goes
;; upwards, here we have a function to correct this behavior:
(define-public (glissando::calc-tab-extra-dy grob)
  (let* ((original (ly:grob-original grob))
         (left-bound (ly:spanner-bound original LEFT))
         (right-bound (ly:spanner-bound original RIGHT))
         (left-pitch (ly:event-property (event-cause left-bound) 'pitch))
         (right-pitch (ly:event-property (event-cause right-bound) 'pitch)))

    (if (< (ly:pitch-semitones right-pitch) (ly:pitch-semitones left-pitch))
        -0.75
        0.75)))

;; for ties in tablature, fret numbers that are tied to should be invisible,
;; except for 'tied to' numbers after a line break;; these will be
;; parenthesized (thanks to Neil for his solution):
(define-public (parenthesize-tab-note-head grob)
  ;; Helper function to parenthesize tab noteheads,
  ;; since we can't use ParenthesesItem at this stage
  ;; This is basically the same as the C++ function
  ;; in accidental.cc, converted to Scheme
  (let* ((font (ly:grob-default-font grob))
         (open (stencil-whiteout
                (ly:font-get-glyph font "accidentals.leftparen")))
         (close (stencil-whiteout
                 (ly:font-get-glyph font "accidentals.rightparen")))
         (me (ly:text-interface::print grob)))

    (ly:stencil-combine-at-edge
     (ly:stencil-combine-at-edge me X LEFT open) X RIGHT close)))

;; ParenthesesItem doesn't work very well for TabNoteHead, since
;; the parentheses are too small and clash with the staff-lines
;; Define a callback for the 'stencils property which will tweak
;; the parentheses' appearance for TabNoteHead
(define-public (parentheses-item::calc-tabstaff-parenthesis-stencils grob)
  ;; the grob we want to parenthesize
  (let ((victim (ly:grob-array-ref (ly:grob-object grob 'elements) 0)))

    ;; check whether it's a note head
    (if (grob::has-interface victim 'note-head-interface)
        (begin
          ;; tweak appearance before retrieving
          ;; list of stencils '(left-paren right-paren)
          ;; get the font-size from victim (=TabNoteHead) to handle
          ;; grace notes properly
          (ly:grob-set-property! grob 'font-size
                                 (ly:grob-property victim 'font-size))
          (ly:grob-set-property! grob 'padding 0)
          ;; apply whiteout to each element of the list
          (map stencil-whiteout
               (parentheses-item::calc-parenthesis-stencils grob)))
        (parentheses-item::calc-parenthesis-stencils grob))))

;; the handler for ties in tablature; according to TabNoteHead #'details,
;; the 'tied to' note is handled differently after a line break
(define-public (tie::handle-tab-note-head grob)
  (let* ((original (ly:grob-original grob))
         (tied-tab-note-head (ly:spanner-bound grob RIGHT))
         (siblings (if (ly:grob? original)
                       (ly:spanner-broken-into original) '())))

    (if (and (>= (length siblings) 2)
             (eq? (car (last-pair siblings)) grob))
        ;; tie is split -> get TabNoteHead #'details
        (let* ((details (ly:grob-property tied-tab-note-head 'details))
               (tied-properties (assoc-get 'tied-properties details '()))
               (tab-note-head-parenthesized (assoc-get 'parenthesize tied-properties #t))
               ;; we need the begin-of-line entry in the 'break-visibility vector
               (tab-note-head-visible
                (vector-ref (assoc-get 'break-visibility
                                       tied-properties #(#f #f #t)) 2)))

              (if tab-note-head-visible
                 ;; tab note head is visible
                 (if tab-note-head-parenthesized
                     (ly:grob-set-property! tied-tab-note-head 'stencil
                                            (lambda (grob)
                                                    (parenthesize-tab-note-head grob))))
                 ;; tab note head is invisible
                 (ly:grob-set-property! tied-tab-note-head 'transparent #t)))

        ;; tie is not split -> make fret number invisible
        (ly:grob-set-property! tied-tab-note-head 'transparent #t))))

;; repeat ties occur within alternatives in a repeat construct;
;; TabNoteHead #'details handles the appearance in this case
(define-public (repeat-tie::handle-tab-note-head grob)
  (let* ((tied-tab-note-head (ly:grob-object grob 'note-head))
         (details (ly:grob-property tied-tab-note-head 'details))
         (repeat-tied-properties (assoc-get 'repeat-tied-properties details '()))
         (tab-note-head-visible (assoc-get 'note-head-visible repeat-tied-properties #t))
         (tab-note-head-parenthesized (assoc-get 'parenthesize repeat-tied-properties #t)))

        (if tab-note-head-visible
            ;; tab note head is visible
            ( if tab-note-head-parenthesized
                 (ly:grob-set-property! tied-tab-note-head 'stencil
                                        (lambda (grob)
                                                (parenthesize-tab-note-head grob))))
            ;; tab note head is invisible
            (ly:grob-set-property! tied-tab-note-head 'transparent #t))))

;; the slurs should not be too far apart from the corresponding fret number, so
;; we move the slur towards the TabNoteHeads:
#(define-public (slur::draw-tab-slur grob)
  ;; TODO: use a less "brute-force" method to decrease
  ;; the distance between the slur ends and the fret numbers
  (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
         (staff-space (ly:grob-property staff-symbol 'staff-space))
         (control-points (ly:grob-property grob 'control-points))
         (new-control-points (map (lambda (p)
                                          (cons (car p) (- (cdr p)
                                                        (* staff-space
                                                           (ly:grob-property grob 'direction)
                                                           0.35))))
                                  control-points)))
        (ly:grob-set-property! grob 'control-points new-control-points)
        (ly:slur::print grob)))