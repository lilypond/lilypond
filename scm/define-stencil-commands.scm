;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
