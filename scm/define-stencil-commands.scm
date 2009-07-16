;;
;; define-stencil-commands.scm -- define valid stencil expression heads
;;
;; source file of the GNU LilyPond music typesetter
;;
;; (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;

;; TODO:
;;  - stencil-commands should have docstrings.

(define-public (ly:all-stencil-commands)
  "Return the list of stencil commands that can be
defined in the output modules (output-*.scm)"
  '(beam
    bezier-sandwich
    blank
    bracket
    char
    circle
    dashed-line
    dashed-slur
    dot
    draw-line
    ellipse
    embedded-ps
    embedded-svg
    glyph-string
    grob-cause
    named-glyph
    no-origin
    oval
    path
    placebox
    polygon
    repeat-slash
    resetcolor
    resetrotation
    round-filled-box
    setcolor
    setrotation
    text
    unknown
    url-link
    utf-8-string
    white-dot
    white-text
    zigzag-line
    ))

;; TODO:
;;  - output-backend-commands should have docstrings.

(define-public (ly:all-output-backend-commands)
  "Return the list of extra output backend commands that
are used internally in lily/stencil-interpret.cc."
  '(color
    combine-stencil
    delay-stencil-evaluation
    rotate-stencil
    translate-stencil
    ))

(map ly:register-stencil-expression
  (append (ly:all-stencil-commands)
	  (ly:all-output-backend-commands)))
