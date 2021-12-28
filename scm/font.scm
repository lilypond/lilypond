;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;; TODO:
;;
;; lookup-font should be written in  C.
;;

;; We have a tree, where each level of the tree is a qualifier
;; (eg. encoding, family, shape, series etc.)  this defines the levels
;; in the tree.  The first one is encoding, so we can directly select
;; between text or music in the first step of the selection.
(define default-qualifier-order
  '(font-encoding font-family font-shape font-series))

(define-class <Font-tree-element>
  ())

(define-class <Font-tree-leaf> (<Font-tree-element>)
  (default-size #:init-keyword #:default-size)
  (size-vector  #:init-keyword #:size-vector))

(define-class <Font-tree-node> (<Font-tree-element>)
  (qualifier #:init-keyword #:qualifier  #:accessor font-qualifier)
  (default #:init-keyword #:default #:accessor font-default)
  (children #:init-keyword #:children #:accessor font-children))

(define (make-font-tree-leaf size size-font-vector)
  (make <Font-tree-leaf> #:default-size size #:size-vector size-font-vector))

(define (make-font-tree-node
         qualifier default)
  (make <Font-tree-node>
    #:qualifier qualifier
    #:default default
    #:children (make-hash-table 11)))

(define-method (display (leaf <Font-tree-leaf>) port)
  (for-each (lambda (x) (display x port))
            (list
             "#<Font-size-family:\n"
             (slot-ref leaf 'default-size)
             (slot-ref leaf 'size-vector)
             "#>"
             )))

(define-method (display (node <Font-tree-node>) port)
  (for-each
   (lambda (x)
     (display x port))
   (list
    "Font_node {\nqual: "
    (font-qualifier node)
    "(def: "
    (font-default node)
    ") {\n"))
  (for-each
   (lambda (x)
     (display "\n")
     (display (car x) port)
     (display "=" port)
     (display (cdr x) port))
   (hash-table->alist (font-children node)))
  (display "} }\n"))


(define-method (add-font (node <Font-tree-node>) fprops size-family)
  (define (assoc-delete key alist)
    (assoc-remove! (list-copy alist) key))

  (define (make-node fprops size-family)
    (if (null? fprops)
        (make-font-tree-leaf (car size-family) (cdr size-family))
        (let* ((qual (next-qualifier default-qualifier-order fprops)))
          (make-font-tree-node qual
                               (assoc-get qual fprops)))))

  (define (next-qualifier order props)
    (cond
     ((and (null? props) (null? order))
      #f)
     ((null? props) (car order))
     ((null? order) (caar props))
     (else
      (if (assoc-get (car order) props)
          (car order)
          (next-qualifier (cdr order) props)))))

  (let* ((q (font-qualifier node))
         (d (font-default node))
         (v (assoc-get q fprops d))
         (new-fprops (assoc-delete q fprops))
         (child (hashq-ref (slot-ref node 'children)
                           v #f)))
    (if (not child)
        (begin
          (set! child (make-node new-fprops size-family))
          (hashq-set! (slot-ref node 'children) v child)))
    (if (pair? new-fprops)
        (add-font child new-fprops size-family))))

(define-method (add-font (node <Font-tree-leaf>) fprops size-family)
  (throw "must add to node, not leaf"))

(define-method (g-lookup-font (node <Font-tree-node>) alist-chain)
  (let* ((qual (font-qualifier node))
         (def (font-default node))
         (val (chain-assoc-get qual alist-chain def))
         (desired-child (hashq-ref (font-children node) val)))

    (if desired-child
        (g-lookup-font desired-child alist-chain)
        (g-lookup-font (hashq-ref (font-children node) def) alist-chain))))

(define-method (g-lookup-font (node <Font-tree-leaf>) alist-chain)
  node)

;; two step call is handy for debugging.
(define (lookup-font node alist-chain)
  (g-lookup-font node alist-chain))

;; TODO - we could actually construct this by loading all OTFs and
;; inspecting their design size fields.
(define-public feta-design-size-mapping
  '((11 . 11.22)
    (13 . 12.60)
    (14 . 14.14)
    (16 . 15.87)
    (18 . 17.82)
    (20 . 20)
    (23 . 22.45)
    (26 . 25.20)))

;; Each size family is a vector of fonts, loaded with a delay.  The
;; vector should be sorted according to ascending design size.
(define-public (add-music-fonts node family name brace design-size-alist factor)
  "Set up music fonts.

Arguments:

@itemize
@item
@var{node} is the font tree to modify.

@item
@var{family} is the family name of the music font.

@item
@var{name} is the basename for the music font.
@file{@var{name}-<designsize>.otf} should be the music font.

@item
@var{brace} is the basename for the brace font.
@file{@var{brace}-brace.otf} should have piano braces.

@item
@var{design-size-alist} is a list of @code{(@var{rounded} .
@var{designsize})}.
@var{rounded} is a suffix for font filenames, while @var{designsize}
should be the actual design size.  The latter is used for text fonts
loaded through pango/@/fontconfig.

@item
@var{factor} is a size factor relative to the default size that is being
used.  This is used to select the proper design size for the text fonts.
@end itemize"
  (for-each
   (lambda (x)
     (add-font node
               (list (cons 'font-encoding (car x))
                     (cons 'font-family family))
               (cons (* factor (cadr x))
                     (caddr x))))

   `((fetaText ,(ly:pt 20.0)
               ,(list->vector
                 (map (lambda (tup)
                        (cons (ly:pt (cdr tup))
                              (format #f "~a-~a ~a"
                                      name
                                      (car tup)
                                      (ly:pt (cdr tup)))))
                      design-size-alist)))
     (fetaMusic ,(ly:pt 20.0)
                ,(list->vector
                  (map (lambda (size-tup)
                         (delay (ly:system-font-load
                                 (format #f "~a-~a" name (car size-tup)))))
                       design-size-alist
                       )))
     (fetaBraces ,(ly:pt 20.0)
                 #(,(delay (ly:system-font-load
                            (format #f "~a-brace" brace)))))
     )))

(define-public (add-pango-fonts node lily-family family factor)
  ;; Synchronized with the `text-font-size' variable in
  ;; layout-set-absolute-staff-size-in-module (see paper.scm).
  (define text-font-size (ly:pt (* factor 11.0)))

  (define (add-node shape series)
    (add-font node
              `((font-family . ,lily-family)
                (font-shape . ,shape)
                (font-series . ,series)
                (font-encoding . latin1) ;; ugh.
                )
              `(,text-font-size
                . #(,(cons
                      (ly:pt 12)
                      (ly:make-pango-description-string
                       `(((font-family . ,family)
                          (font-series . ,series)
                          (font-shape . ,shape)))
                       (ly:pt 12)))))))

  (add-node 'upright 'normal)
  (add-node 'caps 'normal)
  (add-node 'upright 'bold)
  (add-node 'italic 'normal)
  (add-node 'italic 'bold))

;; This function allows the user to change the specific fonts, leaving others
;; to the default values. This way, "make-pango-font-tree"'s syntax doesn't
;; have to change from the user's perspective.
;;
;; Usage:
;;   \paper {
;;     #(define fonts
;;       (set-global-fonts
;;        #:music "gonville"  ; (the main notation font)
;;        #:roman "FreeSerif" ; (the main/serif text font)
;;       ))
;;   }
;;
;; Leaving out "#:brace", "#:sans", and "#:typewriter" leave them at
;; "emmentaler", "sans-serif", and "monospace", respectively. All fonts are
;; still accesible through the usual scheme symbols: 'feta, 'roman, 'sans, and
;; 'typewriter.
;;
;; Note that 'LilyPond Serif', 'LilyPond Sans Serif' and 'Lilypond Monospace'
;; are aliases that are defined in mf/00-lilypond-fonts.conf.in (source file)
;; or fonts/00-lilypond-fonts.conf (installed file).

(define*-public (set-global-fonts #:key
                                  (music "emmentaler")
                                  (brace "emmentaler")
                                  (roman (if (eq? (ly:get-option 'backend) 'svg)
                                             "serif" "LilyPond Serif"))
                                  (sans (if (eq? (ly:get-option 'backend) 'svg)
                                            "sans-serif" "LilyPond Sans Serif"))
                                  (typewriter (if (eq? (ly:get-option 'backend) 'svg)
                                                  "monospace" "LilyPond Monospace"))
                                  (factor 1))
  (let ((n (make-font-tree-node 'font-encoding 'fetaMusic)))
    (add-music-fonts n 'feta music brace feta-design-size-mapping factor)
    (add-pango-fonts n 'roman roman factor)
    (add-pango-fonts n 'sans sans factor)
    (add-pango-fonts n 'typewriter typewriter factor)
    n))

(define-public (make-pango-font-tree roman-str sans-str typewrite-str factor)
  (let ((n (make-font-tree-node 'font-encoding 'fetaMusic)))
    (add-music-fonts n 'feta "emmentaler" "emmentaler" feta-design-size-mapping factor)
    (add-pango-fonts n 'roman roman-str factor)
    (add-pango-fonts n 'sans sans-str factor)
    (add-pango-fonts n 'typewriter typewrite-str factor)
    n))

(define-public (make-default-fonts-tree factor)
  (make-pango-font-tree
   (if (eq? (ly:get-option 'backend) 'svg) "serif" "LilyPond Serif")
   (if (eq? (ly:get-option 'backend) 'svg) "sans-serif" "LilyPond Sans Serif")
   (if (eq? (ly:get-option 'backend) 'svg) "monospace" "LilyPond Monospace")
   factor))

(define-public all-text-font-encodings
  '(latin1))

(define-public all-music-font-encodings
  '(fetaBraces
    fetaMusic
    fetaText))

(define-public (magstep s)
  (exp (* (/ s 6) (log 2))))

(define-public (magnification->font-size m)
  (* 6 (/ (log m) (log 2))))
